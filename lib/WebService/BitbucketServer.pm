package WebService::BitbucketServer;
# ABSTRACT: Bindings for Bitbucket Server REST APIs

=head1 SYNOPSIS

    my $api = WebService::BitbucketServer->new(
        base_url    => 'https://stash.example.com/',
        username    => 'bob',
        password    => 'secret',
    );

    my $response = $api->core->get_application_properties;
    my $app_info = $response->data;
    print "Making API calls to: $app_info->{displayName} $app_info->{version}\n";

    # Or use the low-level method (useful perhaps for new endpoints
    # that are not packaged yet):

    my $response = $api->call(method => 'GET', url => 'api/1.0/application-properties');

    # You can also use your own user agent:

    my $api = WebService::BitbucketServer->new(
        base_url    => 'https://stash.example.com/',
        username    => 'bob',
        password    => 'secret',
        ua          => Mojo::UserAgent->new,
    );

    # If the user agent is nonblocking, responses are Futures:

    my $future = $api->core->get_application_properties;
    $future->on_done(sub {
        my $app_info = shift->data;
        print "Making API calls to: $app_info->{displayName} $app_info->{version}\n";
    });

=head1 DESCRIPTION

This is the main module for the Bitbucket Server API bindings for Perl.

=cut

use warnings;
use strict;

our $VERSION = '9999.999'; # VERSION

use HTTP::AnyUA::Util qw(www_form_urlencode);
use HTTP::AnyUA;
use Module::Load qw(load);
use Scalar::Util qw(weaken);
use Types::Standard qw(Bool Object Str);
use WebService::BitbucketServer::Response;
use WebService::BitbucketServer::Spec qw(api_info documentation_url);

use Moo;
use namespace::clean;

sub _croak { require Carp; Carp::croak(@_) }
sub _usage { _croak("Usage: @_\n") }

sub _debug_log { print STDERR join(' ', @_), "\n" if $ENV{PERL_WEBSERVICE_BITBUCKETSERVER_DEBUG} }

=method new

    $api = WebService::BitbucketServer->new(base_url => $base_url, %other_attributes);

Create a new API context object. Provide L</ATTRIBUTES> to customize.

=attr base_url

Get the base URL of the Bitbucket Server host.

=cut

has base_url => (
    is          => 'ro',
    isa         => Str,
    required    => 1,
);

=attr path

Get the path from the base URL to the APIs. Defaults to "rest".

=cut

has path => (
    is      => 'lazy',
    isa     => Str,
    default => 'rest',
);

=attr username

Get the username of the user for authenticating.

=attr password

Get the password (or personal access token) of the user for authenticating.

=cut

has [qw(username password)] => (
    is  => 'ro',
    isa => Str,
);

=attr ua

Get the user agent used to make API calls.

Defaults to L<HTTP::Tiny>.

Because this API module uses L<HTTP::AnyUA> under the hood, you can actually use any user agent
supported by HTTP::AnyUA.

=cut

has ua => (
    is      => 'lazy',
    default => sub {
        load HTTP::Tiny;
        HTTP::Tiny->new(
            agent   => "perl-webservice-bitbucketserver/$VERSION",
        );
    },
);

=attr any_ua

Get the L<HTTP::AnyUA> object.

=cut

has any_ua => (
    is      => 'lazy',
    isa     => Object,
    default => sub {
        my $self = shift;
        HTTP::AnyUA->new(ua => $self->ua);
    },
);

=attr json

Get the L<JSON> (or compatible) object used for encoding and decoding documents.

=cut

has json => (
    is      => 'lazy',
    isa     => Object,
    default => sub {
        load JSON;
        JSON->new->utf8(1);
    },
);

=attr no_security_warning

Get whether or not a warning will be issued when an insecure action takes place (such as sending
credentials unencrypted). Defaults to false (i.e. will issue warning).

=cut

has no_security_warning => (
    is      => 'rwp',
    isa     => Bool,
    lazy    => 1,
    default => sub { $ENV{PERL_WEBSERVICE_BITBUCKETSERVER_NO_SECURITY_WARNING} || 0 },
);

=method core

Get the L<WebService::BitbucketServer::Core::V1> api.

=method access_tokens

Get the L<WebService::BitbucketServer::AccessTokens::V1> api.

=method audit

Get the L<WebService::BitbucketServer::Audit::V1> api.

=method ref_restriction

Get the L<WebService::BitbucketServer::RefRestriction::V2> api.

=method branch

Get the L<WebService::BitbucketServer::Branch::V1> api.

=method build

Get the L<WebService::BitbucketServer::Build::V1> api.

=method comment_likes

Get the L<WebService::BitbucketServer::CommentLikes::V1> api.

=method default_reviewers

Get the L<WebService::BitbucketServer::DefaultReviewers::V1> api.

=method git

Get the L<WebService::BitbucketServer::Git::V1> api.

=method gpg

Get the L<WebService::BitbucketServer::GPG::V1> api.

=method jira

Get the L<WebService::BitbucketServer::JIRA::V1> api.

=method ssh

Get the L<WebService::BitbucketServer::SSH::V1> api.

=method mirroring_upstream

Get the L<WebService::BitbucketServer::MirroringUpstream::V1> api.

=method repository_ref_sync

Get the L<WebService::BitbucketServer::RepositoryRefSync::V1> api.

=cut

my %api_accessors;
while (my ($namespace, $api) = each %WebService::BitbucketServer::Spec::API) {
    my $method  = $api->{id};
    my $package = __PACKAGE__ . '::' . $api->{package};

    next if $api_accessors{$method};
    $api_accessors{$method} = 1;

    no strict 'refs';   ## no critic ProhibitNoStrict
    *{__PACKAGE__."::${method}"} = sub {
        my $self = shift;
        return $self->{$method} if defined $self->{$method};
        load $package;
        my $api = $package->new(context => $self);
        $self->{$method} = $api;
        weaken($self->{$method});
        return $api;
    };
};

=method url

    $url = $api->url;

Get the URL of the APIs (a combination of L</base_url> and L</path>).

=cut

sub url {
    my $self = shift;
    my $base = $self->base_url;
    my $path = $self->path;
    $base =~ s!/+$!!;
    $path =~ s!^/+!!;
    return "$base/$path";
}

=method call

    $response = $api->call(method => $method, url => $url, %options);

Make a request to an API and get a L<response|WebService::BitbucketServer::Response> (or L<Future>
if the user agent is non-blocking).

=for :list
* url - the endpoint URL, relative to L</url>
* method - the HTTP method
* data - request data
* data_type - type of request data, if any (defaults to "application/json")
* raw - get a hashref response instead of a L<WebService::BitbucketServer::Response>

=cut

sub call {
    my $self = shift;
    (@_ == 1 && ref($_[0]) eq 'HASH') || @_ % 2 == 0
        or _usage(q{$api->call(method => $method, url => $url, %options)});
    my $args = @_ == 1 ? shift : {@_};

    $args->{url} or _croak("url is required\n");

    my $method  = $args->{method} || 'GET';
    my $url     = join('/', $self->url, $args->{url});

    my %options;
    $options{headers}{Accept} = '*/*;q=0.2,application/json';       # prefer json response

    $self->_call_add_authorization($args, \%options);

    # request body
    my $data        = $args->{data};
    my $data_type   = $args->{data_type} || 'application/json';
    if ($data) {
        if ($method eq 'GET' || $method eq 'HEAD') {
            my $params  = ref($data) ? www_form_urlencode($data) : $data;
            my $sep     = $url =~ /\?/ ? '&' : '?';
            $url .= "${sep}${params}";
        }
        else {
            if ($data_type eq 'application/json' && ref($data)) {
                $data = $self->json->encode($data);
            }
            $options{content} = $data;
            $options{headers}{'content-type'}   = $data_type;
            $options{headers}{'content-length'} = length $data;
        }
    }

    my $handle_response = sub {
        my $resp = shift;

        return $resp if $args->{raw};

        return WebService::BitbucketServer::Response->new(
            context         => $self,
            request_args    => $args,
            raw             => $resp,
            json            => $self->json,
        );
    };

    my $resp = $self->any_ua->request($method, $url, \%options);

    if ($self->any_ua->response_is_future) {
        return $resp->transform(
            done => $handle_response,
            fail => $handle_response,
        );
    }
    else {
        return $handle_response->($resp);
    }
}

# add the authorization header to request options
sub _call_add_authorization {
    my $self = shift;
    my $args = shift;
    my $opts = shift;

    if ($self->username && $self->password) {
        my $url = $self->base_url;
        if (!$self->no_security_warning && $url !~ /^https/) {
            warn "Bitbucket Server authorization is being transferred unencrypted to $url !!!\n";
            $self->_set_no_security_warning(0);
        }

        my $payload = $self->username . ':' . $self->password;
        require MIME::Base64;
        my $auth_token = MIME::Base64::encode_base64($payload, '');
        $opts->{headers}{'authorization'} = "Basic $auth_token";
    }
}

=method write_api_packages

    WebService::BitbucketServer->write_api_packages;
    WebService::BitbucketServer->write_api_packages(dir => 'lib');

Download API specifications from L<https://developer.atlassian.com> and generate packages for
them, writing them to the specified directory. You normally don't need this because this module
ships with pre-built APIs, but you can use this to generate other APIs or versions if needed.

Requires L<XML::LibXML>.

=cut

sub write_api_packages {
    my $self = shift;
    (@_ == 1 && ref($_[0]) eq 'HASH') || @_ % 2 == 0
        or _usage(q{$api->write_api_packages(%args)});
    my $args = @_ == 1 ? shift : {@_};

    $self = __PACKAGE__->new(base_url => '') unless ref $self;

    require WebService::BitbucketServer::WADL;

    my $handle_response = sub {
        my $resp = shift;

        if (!$resp->{success}) {
            warn "Failed to fetch $resp->{url} - $resp->{status} $resp->{reason}\n";
            return;
        }

        $self->_debug_log('Fetched WADL', $resp->{url});

        my $wadl = WebService::BitbucketServer::WADL::parse_wadl($resp->{content});

        my $api_info = api_info($wadl);
        if (!$api_info) {
            warn "Missing API info: $resp->{url}\n";
            return;
        }

        my ($package_code, $package) = WebService::BitbucketServer::WADL::generate_package($wadl, %$args, base => __PACKAGE__);

        require File::Path;
        require File::Spec;

        my @pm  = ($args->{dir} ? $args->{dir} : (), _mod_to_pm($package));
        my $pm  = File::Spec->catfile(@pm);
        my $dir = File::Spec->catdir(@pm[0 .. (scalar @pm - 2)]);

        File::Path::make_path($dir);

        # write the pm
        open(my $fh, '>', $pm) or die "open failed ($pm): $!";
        print $fh $package_code;
        close($fh);

        my $submap = WebService::BitbucketServer::WADL::generate_submap($wadl, %$args);

        my $filename = "submap_$api_info->{id}.pl";

        my $filepath = File::Spec->catfile(qw{shares spec}, $filename);
        $dir = File::Spec->catdir(qw{shares spec});

        File::Path::make_path($dir);

        # write the subroutine map
        open($fh, '>', $filepath) or die "open failed ($filepath): $!";
        print $fh $submap;
        close($fh);
    };

    my @responses;
    my %requested;

    for my $namespace (keys %WebService::BitbucketServer::Spec::API) {
        my $url  = documentation_url($namespace, 'wadl', $args->{version});

        next if $requested{$url};
        $requested{$url} = 1;

        my $resp = $self->any_ua->get($url);
        if ($self->any_ua->response_is_future) {
            push @responses, $resp->transform(
                done => $handle_response,
                fail => $handle_response,
            );
        }
        else {
            push @responses, $handle_response->($resp);
        }
    }

    if ($self->any_ua->response_is_future) {
        return Future->wait_all(@responses);
    }
    else {
        return \@responses;
    }
}

sub _mod_to_pm {
    my $mod = shift;
    my @parts = split(/::/, $mod);
    $parts[-1] = "$parts[-1].pm";
    return @parts;
}

1;
