'use strict';

angular.module('web')
    .directive('jqCron',function(){
        return {
            restrict: 'E',
            require: 'ngModel',
            link:function(scope, ele, attr, ngModel){
                if (!ngModel) return;

                var options = {
                    initial: ngModel.$viewValue || "* * * * *",
                    onChange: function () {
                        var value = $(this).cron("value");
                        ngModel.$setViewValue(value);
                    }
                };
                $(ele).cron(options);
            }
        };

    });