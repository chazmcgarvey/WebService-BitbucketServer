# Generated by WebService::BitbucketServer::WADL - DO NOT EDIT!
package WebService::BitbucketServer::CommentLikes::V1;
# ABSTRACT: Bindings for a Bitbucket Server REST API

=head1 SYNOPSIS

    my $stash = WebService::BitbucketServer->new(
        base_url    => 'https://stash.example.com/',
        username    => 'bob',
        password    => 'secret',
    );
    my $api = $stash->comment_likes;

=head1 DESCRIPTION

This is a Bitbucket Server REST API for L<CommentLikes::V1|https://developer.atlassian.com/static/rest/bitbucket-server/5.5.0/bitbucket-comment-likes-rest.html>.

Original API documentation created by and copyright Atlassian.

=cut

use warnings;
use strict;

our $VERSION = '9999.999'; # VERSION

use Moo;
use namespace::clean;

=head1 ATTRIBUTES

=head2 context

Get the instance of L<WebService::BitbucketServer> passed to L</new>.

=cut

has context => (
    is          => 'ro',
    isa         => sub { die 'Not a WebService::BitbucketServer' if !$_[0]->isa('WebService::BitbucketServer'); },
    required    => 1,
);

=head1 METHODS

=head2 new

    $api = WebService::BitbucketServer::CommentLikes::V1->new(context => $webservice_bitbucketserver_obj);

Create a new API.

Normally you would use C<<< $webservice_bitbucketserver_obj->comment_likes >>> instead.

=cut

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

=head2 unlike_commit

Unlike a commit comment in the specified repository, identified by C<<< commitId >>> and
C<<< commentId >>>.

The authenticated user must have the B<<< REPO_READ >>> (or higher) permission for the specified
repository to access this resource.

    DELETE comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/commits/{commitId}/comments/{commentId}/likes

Parameters:

=over 4

=item * C<<< commentId >>> - long, default: none

=item * C<<< commitId >>> - string, default: none

=back

Responses:

=over 4

=item * C<<< 400 >>> - errors, type: application/json

The currently authenticated user is the comment author

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user does not have sufficient permission
(C<<< REPO_READ >>>)

=item * C<<< 204 >>> - data, type: unknown

No content response indicating that the request succeeded

=item * C<<< 404 >>> - errors, type: application/json

The specified repository, commit or comment does not exist

=back

=cut

sub unlike_commit {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/commits/{commitId}/comments/{commentId}/likes', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}

=head2 get_commit_likers

Get a page of users who liked a commit comment in the specified repository, identified by C<<< commitId >>>
and C<<< commentId >>>.

The authenticated user must have the B<<< REPO_READ >>> (or higher) permission for the specified
repository to access this resource.

    GET comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/commits/{commitId}/comments/{commentId}/likes

Parameters:

=over 4

=item * C<<< commentId >>> - long, default: none

=item * C<<< commitId >>> - string, default: none

=back

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

Page of users who liked the specified comment

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user does not have sufficient permission
(C<<< REPO_READ >>>) to query the comment likes

=item * C<<< 404 >>> - errors, type: application/json

The specified repository, commit or comment does not exist

=back

=cut

sub get_commit_likers {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/commits/{commitId}/comments/{commentId}/likes', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head2 like_commit

Like a commit comment in the specified repository, identified by C<<< commitId >>> and
C<<< commentId >>>.

The authenticated user must have the B<<< REPO_READ >>> (or higher) permission for the specified
repository to access this resource.

    POST comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/commits/{commitId}/comments/{commentId}/likes

Parameters:

=over 4

=item * C<<< commentId >>> - long, default: none

=item * C<<< commitId >>> - string, default: none

=back

Responses:

=over 4

=item * C<<< 400 >>> - errors, type: application/json

The currently authenticated user is the comment author

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user does not have sufficient permission
(C<<< REPO_READ >>>).

=item * C<<< 204 >>> - data, type: unknown

No content response indicating that the request succeeded

=item * C<<< 404 >>> - errors, type: application/json

The specified repository, commit or comment does not exist

=back

=cut

sub like_commit {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/commits/{commitId}/comments/{commentId}/likes', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}

=head2 unlike_pull_request

Unlike a pull request comment in the specified repository, identified by C<<< pullRequestId >>> and
C<<< commentId >>>.

The authenticated user must have the B<<< REPO_READ >>> (or higher) permission for the specified
repository to access this resource.

    DELETE comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/pull-requests/{pullRequestId}/comments/{commentId}/likes

Parameters:

=over 4

=item * C<<< commentId >>> - long, default: none

=item * C<<< pullRequestId >>> - long, default: none

=back

Responses:

=over 4

=item * C<<< 400 >>> - errors, type: application/json

The currently authenticated user is the comment author

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user does not have sufficient permission
(C<<< REPO_READ >>>)

=item * C<<< 204 >>> - data, type: unknown

No content response indicating that the request succeeded

=item * C<<< 404 >>> - errors, type: application/json

The specified repository, pull request or comment does not exist

=back

=cut

sub unlike_pull_request {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/pull-requests/{pullRequestId}/comments/{commentId}/likes', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}

=head2 get_pull_request_likers

Get a page of users who liked a pull request comment in the specified repository, identified by
C<<< pullRequestId >>> and C<<< commentId >>>.

The authenticated user must have the B<<< REPO_READ >>> (or higher) permission for the specified
repository to access this resource.

    GET comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/pull-requests/{pullRequestId}/comments/{commentId}/likes

Parameters:

=over 4

=item * C<<< commentId >>> - long, default: none

=item * C<<< pullRequestId >>> - long, default: none

=back

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

Page of users who liked the specified comment

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user does not have sufficient permission
(C<<< REPO_READ >>>) to query the comment likes

=item * C<<< 404 >>> - errors, type: application/json

The specified repository, pull request or comment does not exist

=back

=cut

sub get_pull_request_likers {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/pull-requests/{pullRequestId}/comments/{commentId}/likes', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head2 like_pull_request

Like a pull request comment in the specified repository, identified by C<<< pullRequestId >>> and
C<<< commentId >>>. The like will be recorded against the requesting user.

The authenticated user must have the B<<< REPO_READ >>> (or higher) permission for the specified
repository to access this resource.

    POST comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/pull-requests/{pullRequestId}/comments/{commentId}/likes

Parameters:

=over 4

=item * C<<< commentId >>> - long, default: none

=item * C<<< pullRequestId >>> - long, default: none

=back

Responses:

=over 4

=item * C<<< 400 >>> - errors, type: application/json

The currently authenticated user is the comment author

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user does not have sufficient permission
(C<<< REPO_READ >>>).

=item * C<<< 204 >>> - data, type: unknown

No content response indicating that the request succeeded

=item * C<<< 404 >>> - errors, type: application/json

The specified repository, pull request or comment does not exist

=back

=cut

sub like_pull_request {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('comment-likes/1.0/projects/{projectKey}/repos/{repositorySlug}/pull-requests/{pullRequestId}/comments/{commentId}/likes', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}

=head1 SEE ALSO

=over 4

=item * L<WebService::BitbucketServer>

=item * L<https://developer.atlassian.com/bitbucket/server/docs/latest/>

=back

=cut

1;
