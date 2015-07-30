'use strict';

angular.module('web')
    .service('Auth', function ($rootScope, Users, $q, $http, BASE_URL) {
        var loginUrl = BASE_URL + "rest/session";
        var statuses = {
            404: "Service is unavailable",
            401: "Wrong Credentials"
        };


        var _login = function (email, pass) {
            var deferred = $q.defer();

            var userData = JSON.stringify({
                "email": email,
                "password": pass
            });
            $http.post(loginUrl, userData).success(
                function(data){
                    $http.defaults.headers.common.SessionID = data;
                    $rootScope.sessionId = data;
                    $rootScope.currentUser = email;
                    deferred.resolve();
                })
                .error(function (err, status) {
                    deferred.reject(statuses[status]);
                });

            return deferred.promise;
        };
        
        var _logout= function () {
            return $http.delete(loginUrl + "/" + $rootScope.sessionId)
                .success(function(data){
                    delete $http.defaults.headers.common.SessionID;
                    $rootScope.sessionId = undefined;
                    $rootScope.currentUser = undefined;
                })
            };

        return {
            isLoggedIn: function () {
                return angular.isDefined($rootScope.sessionId);
            },
            logIn: function (email, pass) {
                return _login(email, pass);
            },

            logOut: function () {
                return _logout();
            }
        };
    });
