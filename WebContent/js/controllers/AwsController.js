'use strict';

angular.module('web')
    .controller('AwsController',  function ($scope, $state, Settings) {
        $scope.units = ['GB','TB'];
        $scope.amount = 4;
        $scope.unit = $scope.units[0];

        $scope.changeUnit= function (u) {
            $scope.unit = u;
        };

        $scope.goAhead = function () {
            Settings.save({
                accessKey: $scope.accessKey,
                secretKey: $scope.secretKey,
                bucketSize: $scope.amount + " " + $scope.unit,
                bucketName: $scope.bucketName
            });

            $state.go('app.volume.list');
        }
    });