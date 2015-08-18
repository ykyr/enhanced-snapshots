'use strict';

angular.module('web')
    .directive('stNumberRange', ['$timeout', function ($timeout) {
        return {
            restrict: 'E',
            require: '^stTable',
            scope: {
                lower: '=',
                higher: '='
            },
            templateUrl: './partials/stNumberRange.html',
            controller: 'VolumesController',
            link: function (scope, element, attr, table) {
                var predicateName = attr.predicate;
                var inputs = angular.element.find('input');
                var inputLower = angular.element(inputs[2]);
                var inputHigher = angular.element(inputs[1]);

                    [inputLower, inputHigher].forEach(function (input, index) {

                        input.bind('change', function () {
                            var query = {};

                            if (scope.lower) {
                                query.lower = scope.lower;
                            }

                            if (scope.higher) {
                                query.higher = scope.higher;
                            }

                            scope.$apply(function () {
                                table.search(query, predicateName)
                            });
                        });
                    });
                }
        };
    }]);