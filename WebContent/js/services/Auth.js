'use strict';

angular.module('web')
    .service('Auth', function (Storage, $q, $http, BASE_URL) {
        var sessionUrl = BASE_URL + "rest/session";
        var statuses = {
            404: "Service is unavailable",
            401: "Your authentication information was incorrect. Please try again"
        };


        var _login = function (email, pass) {
            var deferred = $q.defer();

            var userData = JSON.stringify({
                "email": email,
                "password": pass
            });
            $http.post(sessionUrl, userData).success(
                function(data){
                    Storage.save("currentUser", data);
                    deferred.resolve(data);
                })
                .error(function (err, status) {
                    deferred.reject(statuses[status]);
                });

            return deferred.promise;
        };
        
        var _logout= function () {
            Storage.remove("currentUser");
            return $http.delete(sessionUrl)
            };

        return {
            logIn: function (email, pass) {
                return _login(email, pass);
            },

            logOut: function () {
                return _logout();
            }
        };
    });
