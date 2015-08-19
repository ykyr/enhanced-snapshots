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
			// check whether current user has permission to modify existing users
			if (isAdmin(currentUserEmail)) {
				User updatedUser = UserDtoConverter.convert(userInfo);
				if (newPassword != null) {
					updatedUser.setPassword(DigestUtils.sha512Hex(newPassword));
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
			// check whether current user has permission to remove existing users
			if (isAdmin(currentUserEmail)) {
				userRepository.delete(userEmail);
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

}
