/**
 * Created by Administrator on 24.07.2015.
 */
'use strict';

angular.module('web')
    .controller('RegistrationController', function ($scope, $state, Users, $modal) {
        $scope.passwordError = "";
        $scope.userExists = "";
        var userData = {};

        $scope.registerUser = function () {
            userData.name = $scope.userName;
            userData.surname = $scope.userSurname;
            userData.fullname = $scope.userName + ' ' + $scope.userSurname;
            userData.email = $scope.userEmail;
            userData.password = $scope.passwordReg;

            if ($scope.passwordReg === $scope.passwordConf) {

                // check if user already exists
                Users.getAllUsers().then(function (data) {
                    var unique = true;

                    if (data) {
                        for (var i = 0; i < data.length; i++) {
                            if (data[i].email === userData.email) {
                                unique = false;
                                $scope.userExists = "User with such E-mail already exists";
                                break;
                            }
                        }
                    }

                    if (unique) {
                        Users.insert(userData).then(function () {
                            var modalInstance = $modal.open({
                                animation: true,
                                templateUrl: './partials/modal.user-add.html'
                            });

                            modalInstance.result.then(function () {
                                $state.go('login');
                            });
                        });

                    }
                });
            }
            else {
                $scope.passwordError = "Password does not match"
            }
        };
    });




