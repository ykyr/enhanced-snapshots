'use strict';

angular.module('web')
    .directive('jqCron',function(){
        return {
            restrict: 'E',
            require: 'ngModel',
            scope: {
                ngModel: '='
            },
            link:function(scope, ele){
                var options = {
                    initial: scope.ngModel || "* * * * *",
                    onChange: function () {
                        var value = $(this).cron("value");
                        scope.ngModel =value;
                    }
                };
                $(ele).cron(options);
            }
        };

    });