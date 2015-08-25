'use strict';

angular.module('web')
    .directive('stNumberRange', function () {
        return {
            restrict: 'E',
            require: '^stTable',
            scope: {
                lower: '=',
                higher: '=',
            },
            templateUrl: './partials/stNumberRange.html',
            controller: function(){
                debugger;


            }
        };
    });