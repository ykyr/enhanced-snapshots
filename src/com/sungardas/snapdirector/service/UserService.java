package com.sungardas.snapdirector.service;

import com.sungardas.snapdirector.dto.UserDto;


import java.util.List;

public interface UserService {
    List<UserDto> getAllUsers();

    void createUser(UserDto newUser, String password, String currentUserEmail);

    void updateUser(UserDto newUser, String newPassword, String currentUserEmail);

    void removeUser(String userEmail, String currentUserEmail);

    UserDto getUser(String user, String password);

    void deleteAllUsers();

    boolean isTableEmpty();

    boolean isAdmin(String userEmail);
}
