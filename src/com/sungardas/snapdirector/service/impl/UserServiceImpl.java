package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.User;
import com.sungardas.snapdirector.aws.dynamodb.repository.UserRepository;
import com.sungardas.snapdirector.dto.UserDto;
import com.sungardas.snapdirector.dto.converter.UserDtoConverter;
import com.sungardas.snapdirector.exception.DataAccessException;
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
    public void createUser(UserDto userInfo, String password) throws DataAccessException, UniqueConstraintViolationException {
        // check whether user with the same email already exists
        if (userRepository.exists(userInfo.getEmail().toLowerCase())) {
            UniqueConstraintViolationException e = new UniqueConstraintViolationException("User with such email already exists: " + userInfo.getEmail());
            LOG.info("Failed to register user.", e);
            throw e;
        }
        try {
            User newUser = UserDtoConverter.convert(userInfo);
            newUser.setPassword(DigestUtils.sha512Hex(password));
            newUser.setEmail(newUser.getEmail().toLowerCase());
            userRepository.save(newUser);
        } catch (RuntimeException e) {
            LOG.error("Failed to register user.", e);
            throw new DataAccessException(e);
        }
    }
}
