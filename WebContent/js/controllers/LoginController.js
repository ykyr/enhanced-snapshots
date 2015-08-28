'use strict';

angular.module('web')
    .controller('LoginController', function ($scope, $state, Auth, Storage, Settings) {

        if(angular.isDefined(Storage.get("currentUser"))) {
            Auth.logOut();
        }

        $scope.clearErr = function () {
            $scope.error = "";
        };

        $scope.login = function () {
            Auth.logIn($scope.email, $scope.password).then(function () {
                if (Settings.get()) {
                    $state.go('app.volume.list');
                }
                else {
                    $state.go('aws');
                }
            }, function (res) {
                $scope.error = res;
                $scope.password = "";
            });
        }
    });