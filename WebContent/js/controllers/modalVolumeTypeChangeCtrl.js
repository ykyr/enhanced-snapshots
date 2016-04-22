'use strict';

angular.module('web')
    .controller('modalVolumeTypeChangeCtrl', function ($scope, $modalInstance, System) {
        $scope.state = 'ask';

        var volumeTypes = {
            tempVolumeType: $scope.settings.systemProperties.tempVolumeType,
            restoreVolumeType: $scope.settings.systemProperties.restoreVolumeType,
            restoreVolumeIopsPerGb: $scope.settings.systemProperties.restoreVolumeIopsPerGb,
            tempVolumeIopsPerGb: $scope.settings.systemProperties.tempVolumeIopsPerGb
        };

        $scope.changeVolumeType = function () {
            var data = JSON.stringify(volumeTypes);

            System.send(data).then(function () {
                $scope.state = "done";
            }, function (e) {
                $scope.state = "failed";
            });

        }
    });