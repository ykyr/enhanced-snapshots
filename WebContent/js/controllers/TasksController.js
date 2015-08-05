'use strict';

angular.module('web')
    .controller('TasksController', function ($scope, Tasks, $modal) {
        $scope.statusColorClass = {
            running: "primary",
            waiting: "info"
        };
        $scope.typeColorClass = {
            backup: "success",
            restore: "danger"
        };
        $scope.typeIconClass = {
            backup: "cloud-download",
            restore: "cloud-upload"
        };
        $scope.manualIconClass = {
            true: "user",
            false: "time"
        };

        $scope.tasks = [];
        $scope.isLoading = false;
        $scope.refresh = function () {
            $scope.isLoading = true;
            Tasks.get().then(function (data) {
                $scope.tasks = data;
                $scope.isLoading = false;
            }, function () {
                $scope.isLoading = false;
            });

        };
        $scope.refresh();

        $scope.isRunning = function (task) {
            return task.status == "running";
        };

        $scope.reject = function (task) {
            $scope.taskToReject = task;

            var rejectInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.task-reject.html',
                scope: $scope
            });

            rejectInstance.result.then(function () {
                Tasks.delete(task.id).then(function () {
                    $scope.refresh();
                });
            });
        };


    });