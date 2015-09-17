'use strict';

angular.module('web')
    .controller('SettingsController', function ($scope, System, Users, $modal) {
        var currentUser = Users.getCurrent();
        $scope.isAdmin = currentUser.role === "admin";

        System.get().then(function (data) {
            $scope.settings = data;
        }, function (e) {
            console.log(e);
        });

        $scope.backup = function () {
            var modalScope = $scope.$new(true);
        };

        $scope.uninstall = function () {
            $modal.open({
                animation: true,
                templateUrl: './partials/modal.system-uninstall.html',
                controller: 'modalSystemUninstallCtrl'
            });

        };

    });