# Generated by WebService::BitbucketServer::WADL - DO NOT EDIT!
package WebService::BitbucketServer::DefaultReviewers::V1;
# ABSTRACT: Bindings for a Bitbucket Server REST API


use warnings;
use strict;

our $VERSION = '0.603'; # VERSION

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


sub create_pull_request_condition {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('default-reviewers/1.0/projects/{projectKey}/condition', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}


sub delete_pull_request_condition {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('default-reviewers/1.0/projects/{projectKey}/condition/{id}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}


sub update_pull_request_condition {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('default-reviewers/1.0/projects/{projectKey}/condition/{id}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'PUT', url => $url, $data ? (data => $data) : ());
}


sub get_pull_request_conditions {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('default-reviewers/1.0/projects/{projectKey}/conditions', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub create_pull_request_condition_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('default-reviewers/1.0/projects/{projectKey}/repos/{repositorySlug}/condition', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}


sub delete_pull_request_condition_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('default-reviewers/1.0/projects/{projectKey}/repos/{repositorySlug}/condition/{id}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}


sub update_pull_request_condition_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('default-reviewers/1.0/projects/{projectKey}/repos/{repositorySlug}/condition/{id}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'PUT', url => $url, $data ? (data => $data) : ());
}


sub get_pull_request_conditions_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('default-reviewers/1.0/projects/{projectKey}/repos/{repositorySlug}/conditions', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub get_reviewers_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('default-reviewers/1.0/projects/{projectKey}/repos/{repositorySlug}/reviewers', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


1;

__END__

=pod

=encoding UTF-8

=head1 NAME

WebService::BitbucketServer::DefaultReviewers::V1 - Bindings for a Bitbucket Server REST API

=head1 VERSION

version 0.603

=head1 SYNOPSIS

    my $stash = WebService::BitbucketServer->new(
        base_url    => 'https://stash.example.com/',
        username    => 'bob',
        password    => 'secret',
    );
    my $api = $stash->default_reviewers;

=head1 DESCRIPTION

This is a Bitbucket Server REST API for L<DefaultReviewers::V1|https://developer.atlassian.com/static/rest/bitbucket-server/5.5.0/bitbucket-default-reviewers-rest.html>.

Original API documentation created by and copyright Atlassian.

=head1 ATTRIBUTES

=head2 context

Get the instance of L<WebService::BitbucketServer> passed to L</new>.

=head1 METHODS

=head2 new

    $api = WebService::BitbucketServer::DefaultReviewers::V1->new(context => $webservice_bitbucketserver_obj);

Create a new API.

Normally you would use C<<< $webservice_bitbucketserver_obj->default_reviewers >>> instead.

=head2 create_pull_request_condition

Create a default reviewer pull request condition for the given project.

    POST default-reviewers/1.0/projects/{projectKey}/condition

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The default reviewer pull request condition that was created.

=item * C<<< 400 >>> - data, type: application/json

The request was malformed

=back

=head2 delete_pull_request_condition

Delete the default reviewer pull request condition associated with the given ID.

    DELETE default-reviewers/1.0/projects/{projectKey}/condition/{id}

Parameters:

=over 4

=item * C<<< id >>> - int, default: none

The ID of the pull request condition

=back

Responses:

=over 4

=item * C<<< 204 >>> - data, type: unknown

An empty response indicating that the pull request condition was deleted

=item * C<<< 404 >>> - data, type: unknown

An empty response indicating a pull request condition with the given ID could not be found

=back

=head2 update_pull_request_condition

Update the default reviewer pull request condition for the given ID.

    PUT default-reviewers/1.0/projects/{projectKey}/condition/{id}

Parameters:

=over 4

=item * C<<< id >>> - int, default: none

The ID of the pull request condition

=back

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The updated default reviewer pull request condition.

=item * C<<< 400 >>> - data, type: application/json

The request was malformed

=back

=head2 get_pull_request_conditions

Return a page of default reviewer pull request conditions that have been configured for this project.

    GET default-reviewers/1.0/projects/{projectKey}/conditions

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The default reviewer pull request conditions associated with the given project

=back

=head2 create_pull_request_condition_for_repository

Create a default reviewer pull request condition for the given repository.

    POST default-reviewers/1.0/projects/{projectKey}/repos/{repositorySlug}/condition

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The default reviewer pull request condition that was created.

=item * C<<< 400 >>> - data, type: application/json

The request was malformed

=back

=head2 delete_pull_request_condition_for_repository

Delete the default reviewer pull request condition associated with the given ID.

    DELETE default-reviewers/1.0/projects/{projectKey}/repos/{repositorySlug}/condition/{id}

Parameters:

=over 4

=item * C<<< id >>> - int, default: none

The ID of the pull request condition

=back

Responses:

=over 4

=item * C<<< 204 >>> - data, type: unknown

An empty response indicating that the pull request condition was deleted

=item * C<<< 404 >>> - data, type: unknown

An empty response indicating a pull request condition with the given ID could not be found

=back

=head2 update_pull_request_condition_for_repository

Update the default reviewer pull request condition for the given ID.

    PUT default-reviewers/1.0/projects/{projectKey}/repos/{repositorySlug}/condition/{id}

Parameters:

=over 4

=item * C<<< id >>> - int, default: none

The ID of the pull request condition

=back

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The updated default reviewer pull request condition.

=item * C<<< 400 >>> - data, type: application/json

The request was malformed

=back

=head2 get_pull_request_conditions_for_repository

Return a page of default reviewer pull request conditions that have been configured for this repository.

    GET default-reviewers/1.0/projects/{projectKey}/repos/{repositorySlug}/conditions

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The default reviewer pull request conditions associated with the given repository

=back

=head2 get_reviewers_for_repository

Return a set of users who are required reviewers for pull requests created from the given source repository
and ref to the given target ref in this repository.

    GET default-reviewers/1.0/projects/{projectKey}/repos/{repositorySlug}/reviewers

Parameters:

=over 4

=item * C<<< sourceRepoId >>> - int, default: none

The ID of the repository in which the source ref exists

=item * C<<< targetRepoId >>> - int, default: none

The ID of the repository in which the target ref exists

=item * C<<< sourceRefId >>> - string, default: none

The ID of the source ref

=item * C<<< targetRefId >>> - string, default: none

The ID of the target ref

=back

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The default reviewer pull request conditions associated with the given
repository and refs

=item * C<<< 400 >>> - data, type: application/json

The request was malformed

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

This software is copyright (c) 2017 by Charles McGarvey.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
