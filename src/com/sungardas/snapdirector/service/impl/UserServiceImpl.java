package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.Roles;
import com.sungardas.snapdirector.aws.dynamodb.model.User;
import com.sungardas.snapdirector.aws.dynamodb.repository.UserRepository;
import com.sungardas.snapdirector.dto.UserDto;
import com.sungardas.snapdirector.dto.converter.UserDtoConverter;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.exception.OperationNotAllowedException;
import com.sungardas.snapdirector.exception.UniqueConstraintViolationException;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.UserService;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.List;

@Service
@DependsOn("CreateAppConfiguration")
public class UserServiceImpl implements UserService {

    private static final Logger LOG = LogManager.getLogger(UserServiceImpl.class);

    private String instanceId;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ConfigurationService configurationService;

    @PostConstruct
    private void init() {
        instanceId = configurationService.getConfiguration().getConfigurationId();
    }

    @Override
    public List<UserDto> getAllUsers() {
        try {
            return UserDtoConverter.convert(userRepository.findByInstanceId(instanceId));
        } catch (RuntimeException e) {
            LOG.error(e);
            throw new DataAccessException(e);
        }
    }

    @Override
    public void createUser(UserDto userInfo, String password, String currentUserEmail) throws DataAccessException, UniqueConstraintViolationException {
        // check whether user with the same email already exists
        if (!userRepository.findByEmailAndInstanceId(userInfo.getEmail().toLowerCase(), instanceId).isEmpty()) {
            UniqueConstraintViolationException e = new UniqueConstraintViolationException("User with such email already exists: " + userInfo.getEmail());
            LOG.info("Failed to register user.", e);
            throw e;
        }
        try {
            // check whether current user has permission to create new users
            if (isAdmin(currentUserEmail)) {
                User newUser = UserDtoConverter.convert(userInfo);
                newUser.setPassword(DigestUtils.sha512Hex(password));

                // convert user email to lowercase
                newUser.setEmail(newUser.getEmail().toLowerCase());
                if (newUser.getRole().isEmpty()) {
                    newUser.setRole(Roles.USER.getName());
                }
                newUser.setInstanceId(instanceId);
                userRepository.save(newUser);
            } else {
                OperationNotAllowedException e = new OperationNotAllowedException("Only users with admin role can create new user.");
                LOG.info("Failed to register user.", e);
                throw e;
            }
        } catch (RuntimeException e) {
            LOG.error("Failed to register user.", e);
            throw e;
        }
    }

    @Override
    public void updateUser(UserDto userInfo, String newPassword, String currentUserEmail) {
        // check user exists
        List<User> users = userRepository.findByEmailAndInstanceId(userInfo.getEmail().toLowerCase(), instanceId);
        if (users.isEmpty()) {
            DataAccessException e = new DataAccessException("User with such email does not exist: " + userInfo.getEmail());
            LOG.info("Failed to update user.", e);
            throw e;
        }
        try {
            // currentUser - user who performs update
            User currentUser = userRepository.findByEmailAndInstanceId(currentUserEmail.toLowerCase(), instanceId).get(0);

            // check whether current user has permission to modify existing user
            if (isAdmin(currentUser) || userInfo.getEmail().toLowerCase().equals(currentUserEmail)) {
                User userToBeUpdated = users.get(0);
                User updatedUser = UserDtoConverter.convert(userInfo);
                updatedUser.setId(userToBeUpdated.getId());
                // new password setting
                if (newPassword != null && !newPassword.isEmpty()) {
                    updatedUser.setPassword(DigestUtils.sha512Hex(newPassword));
                } else {
                    // in case password is empty we use old password
                    updatedUser.setPassword(userToBeUpdated.getPassword());
                }
                // in case it's last admin in system, ADMIN role can not be changed
                if (isAdmin(userToBeUpdated) && updatedUser.getRole().equals(Roles.USER.getName()) && isLastAdmin()) {
                    OperationNotAllowedException e = new OperationNotAllowedException("At least one user with admin role must be in system.");
                    LOG.error("Admin role can not be changed in case it's last admin in system.", e);
                    throw e;
                }
                // not admin user can not change its role to admin
                if (!isAdmin(currentUser) && updatedUser.getRole().equals(Roles.ADMIN.getName())) {
                    OperationNotAllowedException e = new OperationNotAllowedException("Only admin users can create other users with admin roles");
                    LOG.error("Only admin users can create other users with admin roles", e);
                    throw e;
                }
                // convert user email to lowercase
                updatedUser.setEmail(updatedUser.getEmail().toLowerCase());
                userRepository.save(updatedUser);
            } else {
                OperationNotAllowedException e = new OperationNotAllowedException("Only users with admin role can update users.");
                LOG.error("Failed to update user.", e);
                throw e;
            }
        } catch (RuntimeException e) {
            LOG.error("Failed to update user.", e);
            throw e;
        }
    }

    @Override
    public void removeUser(String userEmail, String currentUserEmail) {
        // check user exists
        if (userRepository.findByEmailAndInstanceId(userEmail.toLowerCase(), instanceId).isEmpty()) {
            DataAccessException e = new DataAccessException("User with such email does not exist: " + userEmail);
            LOG.info("Failed to remove user.", e);
            throw e;
        }
        try {
            // check whether current user has permission to remove existing users and it is not last admin in system
            if (isAdmin(currentUserEmail)) {
                // in case it's last admin in system, it can not be removed
                if (isAdmin(userEmail) && isLastAdmin()) {
                    OperationNotAllowedException e = new OperationNotAllowedException("Admin user can not be removed in case it is the last admin in system.");
                    LOG.debug("Admin user can not be removed in case it's last admin in system.", e);
                    throw e;
                } else {
                    userRepository.delete(userEmail);
                }
            } else {
                OperationNotAllowedException e = new OperationNotAllowedException("Only users with admin role can remove users.");
                LOG.info("Failed to remove user.", e);
                throw e;
            }
        } catch (RuntimeException e) {
            LOG.error("Failed to remove user.", e);
            throw e;
        }

    }

    @Override
    public UserDto getUser(String user, String password) {
        List<UserDto> list = UserDtoConverter.convert(userRepository.findByEmailAndPasswordAndInstanceId(user, DigestUtils.sha512Hex(password), instanceId));
        if (list.isEmpty()) {
            return null;
        } else {
            return list.get(0);
        }
    }

    @Override
    public void deleteAllUsers() {
        List<User> userList = userRepository.findByInstanceId(instanceId);
        userRepository.delete(userList);
    }

    public boolean isAdmin(String userEmail) {
        User user = userRepository.findByEmailAndInstanceId(userEmail.toLowerCase(), instanceId).get(0);
        return user.getRole() != null && user.getRole().equals(Roles.ADMIN.getName());
    }

    private boolean isAdmin(User user) {
        return user.getRole() != null &&
                user.getRole().equals(Roles.ADMIN.getName());
    }

    private boolean isLastAdmin() {
        return userRepository.findByRoleAndInstanceId(Roles.ADMIN.getName(), instanceId).size() == 1;
    }
}
