/**
 * Created by avas on 31.07.2015.
 */

angular.module('web')
    .factory('Interceptor', function ($q) {

        return {
            responseError: function (rejection) {
                if (rejection.status === 401) {
                    window.location = "#/login";
                } else if (rejection.status === 404) {
                    window.location = "#/login";
                }
                return $q.reject(rejection);
            }
        }

    });