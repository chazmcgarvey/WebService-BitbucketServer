# NAME

WebService::BitbucketServer - Bindings for Bitbucket Server REST APIs

# VERSION

version 0.604

# SYNOPSIS

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

# DESCRIPTION

This is the main module for the Bitbucket Server API bindings for Perl.

# ATTRIBUTES

## base\_url

Get the base URL of the Bitbucket Server host.

## path

Get the path from the base URL to the APIs. Defaults to "rest".

## username

Get the username of the user for authenticating.

## password

Get the password of the user for authenticating.

## ua

Get the user agent used to make API calls.

Defaults to [HTTP::Tiny](https://metacpan.org/pod/HTTP::Tiny).

Because this API module uses [HTTP::AnyUA](https://metacpan.org/pod/HTTP::AnyUA) under the hood, you can actually use any user agent
supported by HTTP::AnyUA.

## any\_ua

Get the [HTTP::AnyUA](https://metacpan.org/pod/HTTP::AnyUA) object.

## json

Get the [JSON](https://metacpan.org/pod/JSON) (or compatible) object used for encoding and decoding documents.

## no\_security\_warning

Get whether or not a warning will be issued when an insecure action takes place (such as sending
credentials unencrypted). Defaults to false (i.e. will issue warning).

# METHODS

## new

    $api = WebService::BitbucketServer->new(base_url => $base_url, %other_attributes);

Create a new API context object. Provide ["ATTRIBUTES"](#attributes) to customize.

## core

Get the [WebService::BitbucketServer::Core::V1](https://metacpan.org/pod/WebService::BitbucketServer::Core::V1) api.

## access\_tokens

Get the [WebService::BitbucketServer::AccessTokens::V1](https://metacpan.org/pod/WebService::BitbucketServer::AccessTokens::V1) api.

## audit

Get the [WebService::BitbucketServer::Audit::V1](https://metacpan.org/pod/WebService::BitbucketServer::Audit::V1) api.

## ref\_restriction

Get the [WebService::BitbucketServer::RefRestriction::V2](https://metacpan.org/pod/WebService::BitbucketServer::RefRestriction::V2) api.

## branch

Get the [WebService::BitbucketServer::Branch::V1](https://metacpan.org/pod/WebService::BitbucketServer::Branch::V1) api.

## build

Get the [WebService::BitbucketServer::Build::V1](https://metacpan.org/pod/WebService::BitbucketServer::Build::V1) api.

## comment\_likes

Get the [WebService::BitbucketServer::CommentLikes::V1](https://metacpan.org/pod/WebService::BitbucketServer::CommentLikes::V1) api.

## default\_reviewers

Get the [WebService::BitbucketServer::DefaultReviewers::V1](https://metacpan.org/pod/WebService::BitbucketServer::DefaultReviewers::V1) api.

## git

Get the [WebService::BitbucketServer::Git::V1](https://metacpan.org/pod/WebService::BitbucketServer::Git::V1) api.

## gpg

Get the [WebService::BitbucketServer::GPG::V1](https://metacpan.org/pod/WebService::BitbucketServer::GPG::V1) api.

## jira

Get the [WebService::BitbucketServer::JIRA::V1](https://metacpan.org/pod/WebService::BitbucketServer::JIRA::V1) api.

## ssh

Get the [WebService::BitbucketServer::SSH::V1](https://metacpan.org/pod/WebService::BitbucketServer::SSH::V1) api.

## mirroring\_upstream

Get the [WebService::BitbucketServer::MirroringUpstream::V1](https://metacpan.org/pod/WebService::BitbucketServer::MirroringUpstream::V1) api.

## repository\_ref\_sync

Get the [WebService::BitbucketServer::RepositoryRefSync::V1](https://metacpan.org/pod/WebService::BitbucketServer::RepositoryRefSync::V1) api.

## url

    $url = $api->url;

Get the URL of the APIs (a combination of ["base\_url"](#base_url) and ["path"](#path)).

## call

    $response = $api->call(method => $method, url => $url, %options);

Make a request to an API and get a [response](https://metacpan.org/pod/WebService::BitbucketServer::Response) (or [Future](https://metacpan.org/pod/Future)
if the user agent is non-blocking).

- url - the endpoint URL, relative to ["url"](#url)
- method - the HTTP method
- data - request data
- data\_type - type of request data, if any (defaults to "application/json")
- raw - get a hashref response instead of a [WebService::BitbucketServer::Response](https://metacpan.org/pod/WebService::BitbucketServer::Response)

## write\_api\_packages

    WebService::BitbucketServer->write_api_packages;
    WebService::BitbucketServer->write_api_packages(dir => 'lib');

Download API specifications from [https://developer.atlassian.com](https://developer.atlassian.com) and generate packages for
them, writing them to the specified directory. You normally don't need this because this module
ships with pre-built APIs, but you can use this to generate other APIs or versions if needed.

Requires [XML::LibXML](https://metacpan.org/pod/XML::LibXML).

# BUGS

Please report any bugs or feature requests on the bugtracker website
[https://github.com/chazmcgarvey/WebService-BitbucketServer/issues](https://github.com/chazmcgarvey/WebService-BitbucketServer/issues)

When submitting a bug or request, please include a test-file or a
patch to an existing test-file that illustrates the bug or desired
feature.

# AUTHOR

Charles McGarvey <chazmcgarvey@brokenzipper.com>

# CONTRIBUTOR

Camspi <amarus18@hotmail.com>

# COPYRIGHT AND LICENSE

This software is copyright (c) 2018 by Charles McGarvey.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.
