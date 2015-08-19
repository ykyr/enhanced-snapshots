package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.Roles;
import com.sungardas.snapdirector.aws.dynamodb.model.User;
import com.sungardas.snapdirector.aws.dynamodb.repository.UserRepository;
import com.sungardas.snapdirector.dto.UserDto;
import com.sungardas.snapdirector.dto.converter.UserDtoConverter;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.exception.OperationNotAllowedException;
import com.sungardas.snapdirector.exception.UniqueConstraintViolationException;
import com.sungardas.snapdirector.service.UserService;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class UserServiceImpl implements UserService {

	@Autowired
	private UserRepository userRepository;

	private static final Logger LOG = LogManager.getLogger(UserServiceImpl.class);

	@Override
	public List<UserDto> getAllUsers() {
		try {
			return UserDtoConverter.convert(userRepository.findAll());
		} catch (RuntimeException e) {
			LOG.error(e);
			throw new DataAccessException(e);
		}
	}

	@Override
	public void createUser(UserDto userInfo, String password, String currentUserEmail) throws DataAccessException, UniqueConstraintViolationException {
		// check whether user with the same email already exists
		if (userRepository.exists(userInfo.getEmail().toLowerCase())) {
			UniqueConstraintViolationException e = new UniqueConstraintViolationException("User with such email already exists: " + userInfo.getEmail());
			LOG.info("Failed to register user.", e);
			throw e;
		}
		try {
			// check whether current user has permission to create new users
			if (isAdmin(currentUserEmail)) {
				User newUser = UserDtoConverter.convert(userInfo);
				newUser.setPassword(DigestUtils.sha512Hex(password));
				newUser.setEmail(newUser.getEmail().toLowerCase());
				if (newUser.getRole().isEmpty()) {
					newUser.setRole(Roles.USER.getName());
				}
				userRepository.save(newUser);
			} else {
				OperationNotAllowedException e = new OperationNotAllowedException("Only users with admin role can create new user.");
				LOG.info("Failed to register user.", e);
				throw e;
			}
		} catch (RuntimeException e) {
			LOG.error("Failed to register user.", e);
			throw new DataAccessException(e);
		}
	}

	@Override
	public void updateUser(UserDto userInfo, String newPassword, String currentUserEmail) {
		// check user exists
		if (!userRepository.exists(userInfo.getEmail().toLowerCase())) {
			DataAccessException e = new DataAccessException("User with such email does not exist: " + userInfo.getEmail());
			LOG.info("Failed to update user.", e);
			throw e;
		}
		try {
			// check whether current user has permission to modify existing user
			if ((isAdmin(currentUserEmail) || userInfo.getEmail().toLowerCase().equals(currentUserEmail))) {
				User userToBeUpdated = userRepository.findOne(userInfo.getEmail().toLowerCase());
				User updatedUser = UserDtoConverter.convert(userInfo);
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
					LOG.debug("Admin role can not be changed in case it's last admin in system.");
					throw e;
				}
				userRepository.save(updatedUser);
			} else {
				OperationNotAllowedException e = new OperationNotAllowedException("Only users with admin role can update users.");
				LOG.info("Failed to update user.", e);
				throw e;
			}
		} catch (RuntimeException e) {
			LOG.error("Failed to update user.", e);
			throw new DataAccessException(e);
		}
	}

	@Override
	public void removeUser(String userEmail, String currentUserEmail) {
		// check user exists
		if (!userRepository.exists(userEmail.toLowerCase())) {
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
			throw new DataAccessException(e);
		}

	}

	private boolean isAdmin(String userEmail) {
		return userRepository.findOne(userEmail.toLowerCase()).getRole() != null &&
				userRepository.findOne(userEmail.toLowerCase()).getRole().equals(Roles.ADMIN.getName());
	}

	private boolean isAdmin(User user) {
		return user.getRole() != null &&
				user.getRole().equals(Roles.ADMIN.getName());
	}

	private boolean isLastAdmin() {
		return userRepository.findByRole(Roles.ADMIN.getName()).size() == 1;
	}
}
