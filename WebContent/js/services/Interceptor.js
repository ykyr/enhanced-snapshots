/**
 * Created by avas on 31.07.2015.
 */

angular.module('web')
    .factory('Interceptor', function ($q, Exception) {

        return {
            responseError: function (rejection) {
                if (rejection.status === 401) {
                    window.location = "#/login";
                } else if (rejection.status === 500 && rejection.data.localizedMessage){
                    Exception.handle(rejection);
                }
                return $q.reject(rejection);
            }
        }
    });