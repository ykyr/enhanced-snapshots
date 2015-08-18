'use strict';

angular.module('web')
    .directive("repeatPassword", function () {
        return {
            restrict: "A",
            require: "ngModel",
            link: function (scope, elem, attrs, ctrl) {

                var checker = function () {
                    var passwordNew = scope.$eval(attrs.repeatPassword);
                    var passwordConfirm = scope.$eval(attrs.ngModel);
                    return passwordNew === passwordConfirm;
                };

                elem.bind('input', function () {
                    scope.$watch(checker, function (n) {
                        ctrl.$setValidity('match', n)
                    })
                })
            }
        };
    });