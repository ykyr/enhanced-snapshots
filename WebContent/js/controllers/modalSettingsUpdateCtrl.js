'use strict';

angular.module('web')
    .controller('modalVolumeTypeChangeCtrl', function ($scope, $modalInstance, System, TasksController) {
        $scope.state = 'ask';

        var newSettings = $scope.settings;
        //deletion of Arrays from model as per request of backend
        delete newSettings.s3.suffixesInUse;
        delete newSettings.systemProperties.volumeTypeOptions;

        var abc = TasksController.tasks;


        $scope.changeVolumeType = function () {
            var data = JSON.stringify(newSettings);

            System.send(data).then(function () {
                $scope.state = "done";
            }, function (e) {
                $scope.state = "failed";
            });

        }
    });