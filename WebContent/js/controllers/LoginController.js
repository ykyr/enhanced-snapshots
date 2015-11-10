'use strict';

angular.module('web')
    .controller('LoginController', function ($scope, $state, $stateParams, Auth, Storage, toastr) {

        if ($stateParams.err && $stateParams.err == 'session') {
            toastr.warning('You was loged out. Please relogin.', 'Session expired.');
        }

        if (angular.isDefined(Storage.get("currentUser"))) {
            Auth.logOut();
        }

        if (Storage.get("currentUser") && Storage.get("currentUser").length > 1) {
            Auth.logOut();
        }

        $scope.clearErr = function () {
            $scope.error = "";
        };

        $scope.login = function () {
            Auth.logIn($scope.email, $scope.password).then(function (data) {

                if (data.role === 'configurator') {
                    $state.go('config');
                } else {
                    $state.go('app.volume.list');
                }
            }, function (res) {
                $scope.error = res;
                $scope.password = "";
            });
        }
    });