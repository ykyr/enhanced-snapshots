'use strict';

angular.module('web')
    .service('Auth', function ($rootScope, Users, $q, BASE_URL) {
        // var loginUrl = BASE_URL + "rest/user";
        var authenticate = function (email, pass) {
            var deferred = $q.defer();

            Users.getAllUsers().then(function (data) {
                var isPresent;
                for (var i = 0; i < data.length; i++) {
                    if (data[i].email == email && data[i].password == pass) {
                        $rootScope.currentUser = data[i];
                        deferred.resolve(data);
                        isPresent = true;
                        break;
                    }
                }

                if (!isPresent) {
                    deferred.reject('Wrong credentials');
                }
            });
            return deferred.promise;
        };

        var _hasUsers = function () {
            var deferred = $q.defer();

            Users.getAllUsers().then(function (data) {
                deferred.resolve(data.length > 0);
            }, function () {
                deferred.reject('Error getting users');
            });
            return deferred.promise;
        };

        return {
            isLoggedIn: function () {
                if (angular.isDefined($rootScope.currentUser)) {
                    return $rootScope.currentUser;
                } else {
                    return false;
                }
            },
            hasUsers: function () {
                return _hasUsers();
            },
            logIn: function (email, pass) {
                return authenticate(email, pass);
            },
            logOut: function () {
                $rootScope.currentUser = undefined;
                return true;
            }
        };
    });