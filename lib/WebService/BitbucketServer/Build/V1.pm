# Generated by WebService::BitbucketServer::WADL - DO NOT EDIT!
package WebService::BitbucketServer::Build::V1;
# ABSTRACT: Bindings for a Bitbucket Server REST API


use warnings;
use strict;

our $VERSION = '0.605'; # VERSION

use Moo;
use namespace::clean;


has context => (
    is          => 'ro',
    isa         => sub { die 'Not a WebService::BitbucketServer' if !$_[0]->isa('WebService::BitbucketServer'); },
    required    => 1,
);


sub _croak { require Carp; Carp::croak(@_) }

sub _get_url {
    my $url  = shift;
    my $args = shift || {};
    $url =~ s/\{([^:}]+)(?::\.\*)?\}/_get_path_parameter($1, $args)/eg;
    return $url;
}

sub _get_path_parameter {
    my $name = shift;
    my $args = shift || {};
    return delete $args->{$name} if defined $args->{$name};
    $name =~ s/([A-Z])/'_'.lc($1)/eg;
    return delete $args->{$name} if defined $args->{$name};
    _croak("Missing required parameter $name");
}


sub get_multiple_build_status_stats {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('build-status/1.0/commits/stats', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}


sub get_build_status_stats {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('build-status/1.0/commits/stats/{commitId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub get_build_status {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('build-status/1.0/commits/{commitId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub add_build_status {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('build-status/1.0/commits/{commitId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}


1;

__END__

=pod

=encoding UTF-8

=head1 NAME

WebService::BitbucketServer::Build::V1 - Bindings for a Bitbucket Server REST API

=head1 VERSION

version 0.605

=head1 SYNOPSIS

    my $stash = WebService::BitbucketServer->new(
        base_url    => 'https://stash.example.com/',
        username    => 'bob',
        password    => 'secret',
    );
    my $api = $stash->build;

=head1 DESCRIPTION

This is a Bitbucket Server REST API for L<Build::V1|https://developer.atlassian.com/static/rest/bitbucket-server/5.10.0/bitbucket-build-rest.html>.

Original API documentation created by and copyright Atlassian.

=head1 ATTRIBUTES

=head2 context

Get the instance of L<WebService::BitbucketServer> passed to L</new>.

=head1 METHODS

=head2 new

    $api = WebService::BitbucketServer::Build::V1->new(context => $webservice_bitbucketserver_obj);

Create a new API.

Normally you would use C<<< $webservice_bitbucketserver_obj->build >>> instead.

=head2 get_multiple_build_status_stats

Produces a list of the build statistics for multiple commits.

Commits I<<< without any builds associated with them >>> will not be returned. For example if the
commit C<<< e00cf62997a027bbf785614a93e2e55bb331d268 >>> does not have any build statuses associated
with it, it will not be present in the response.

    POST build-status/1.0/commits/stats

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The number of successful/failed/in-progress builds for each commit
(with the caveat that the commits I<<< without any builds associated with
them >>> will not be present in the response)

=item * C<<< 401 >>> - errors, type: application/json

The user is not authenticated or does not have the B<<< LICENSED >>> permission.

=back

=head2 get_build_status_stats

Gets statistics regarding the builds associated with a commit.

    GET build-status/1.0/commits/stats/{commitId}

Parameters:

=over 4

=item * C<<< commitId >>> - string, default: none

full SHA1 of the commit (ex: C<<< e00cf62997a027bbf785614a93e2e55bb331d268 >>>)

=item * C<<< includeUnique >>> - boolean, default: false

include a unique build result if there is either only one failed build,
only one in-progress build or only one successful build

=back

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The number of successful/failed/in-progress builds for the commit

=item * C<<< 401 >>> - errors, type: application/json

The user is not authenticated or does not have the B<<< LICENSED >>> permission.

=back

=head2 get_build_status

Gets the build statuses associated with a commit.

    GET build-status/1.0/commits/{commitId}

Parameters:

=over 4

=item * C<<< commitId >>> - string, default: none

full SHA1 of the commit (ex: C<<< e00cf62997a027bbf785614a93e2e55bb331d268 >>>)

=back

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

a page of build statuses associated with the commit
(limited to the most recent 100 build statuses associated with the commit)

=item * C<<< 401 >>> - errors, type: application/json

The user is not authenticated or does not have the B<<< LICENSED >>> permission.

=back

=head2 add_build_status

Associates a build status with a commit.

The C<<< state >>>, the C<<< key >>> and the C<<< url >>> are mandatory. The C<<< name >>> and
C<<< description >>> fields are optional.

All fields (mandatory or optional) are limited to 255 characters, except for the C<<< url >>>,
which is limited to 450 characters.

Supported values for the C<<< state >>> are C<<< SUCCESSFUL >>>, C<<< FAILED >>>
and C<<< INPROGRESS >>>.

The authenticated user must have B<<< LICENSED >>> permission or higher to call this resource.

    POST build-status/1.0/commits/{commitId}

Parameters:

=over 4

=item * C<<< commitId >>> - string, default: none

full SHA1 of the commit (ex: C<<< e00cf62997a027bbf785614a93e2e55bb331d268 >>>)

=back

Responses:

=over 4

=item * C<<< 400 >>> - errors, type: application/json

An error message if the C<<< commitId >>> if not a full 40-characters SHA1,
if the build status has a missing mandatory field or if the fields are too large

=item * C<<< 401 >>> - errors, type: application/json

The user is not authenticated or does not have the B<<< LICENSED >>> permission.

=item * C<<< 204 >>> - data, type: unknown

An empty response if the build status was successfully stored

=back

=head1 SEE ALSO

=over 4

=item * L<WebService::BitbucketServer>

=item * L<https://developer.atlassian.com/bitbucket/server/docs/latest/>

=back

=head1 BUGS

Please report any bugs or feature requests on the bugtracker website
L<https://github.com/chazmcgarvey/WebService-BitbucketServer/issues>

When submitting a bug or request, please include a test-file or a
patch to an existing test-file that illustrates the bug or desired
feature.

=head1 AUTHOR

Charles McGarvey <chazmcgarvey@brokenzipper.com>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2018 by Charles McGarvey.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
