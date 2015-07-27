'use strict';

angular.module('web')
    .service('Auth', function ($http, $state, $rootScope, $q) {
        var loginUrl = "./rest/user";
        var authenticate = function (name, pass) {
            var deferred = $q.defer();

            $http.get(loginUrl).success(function (data) {
                if (data.name == name && data.password == pass){
                    $rootScope.currentUser = data;
                    deferred.resolve(data);
                }else{
                    deferred.reject('Wrong credentials');
                }
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
            logIn: function (name, pass) {
                return authenticate(name, pass);
            },
            logOut: function (){
                $rootScope.currentUser = undefined;
                return true;
            }
        };
    });