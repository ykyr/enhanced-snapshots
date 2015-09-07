'use strict';

angular.module('web')
    .controller('TasksController', function ($scope, Tasks, $modal) {
        $scope.statusColorClass = {
            "waiting": "",
            "queued": "info",
            "running": "primary",
            "completed": "success",
            "error": "danger"
        };
        $scope.typeColorClass = {
            backup: "primary",
            restore: "success",
            delete: "danger"

        };
        $scope.typeIconClass = {
            backup: "cloud-download",
            restore: "cloud-upload",
            delete: "remove"
        };
        $scope.manualIconClass = {
            true: "user",
            false: "time"
        };

        $scope.statusPriority = function (task) {
            var priorities = {
                running: 4,
                queued: 3,
                error: 2,
                waiting: 1
            };
            return priorities[task.status] || 0;
        };

        $scope.tasks = [];
        $rootScope.isLoading = false;
        $scope.refresh = function () {
            $rootScope.isLoading = true;
            Tasks.get().then(function (data) {
                $scope.tasks = data;
                $rootScope.isLoading = false;
            }, function () {
                $rootScope.isLoading = false;
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