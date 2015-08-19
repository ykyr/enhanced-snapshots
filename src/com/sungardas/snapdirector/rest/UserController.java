package com.sungardas.snapdirector.rest;

import com.sungardas.snapdirector.dto.UserDto;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.rest.utils.Constants;
import com.sungardas.snapdirector.service.UserService;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.node.ObjectNode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.Collections;
import java.util.Map;


@RestController
@RequestMapping("/user")
public class UserController {

	@Autowired
	private UserService userService;
	@Autowired
	private ServletContext context;
	@Autowired
	private HttpServletRequest servletRequest;

	private ObjectMapper mapper;

	@RequestMapping(method = RequestMethod.POST)
	public ResponseEntity<String> createUser(@RequestBody String userInfo) {
		try {
			// getting userDto from json
			UserDto user = getUserDtoFromJson(userInfo);
			// getting password
			String password = mapper.readValue(userInfo, ObjectNode.class).get("password").asText();
			userService.createUser(user, password, getCurrentUserEmail());
			return new ResponseEntity<>("", HttpStatus.OK);
		} catch (SnapdirectorException e) {
			return new ResponseEntity<>(e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
		} catch (IOException e) {
			return new ResponseEntity<>("Failed to parse json", HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@RequestMapping(method = RequestMethod.PUT)
	public ResponseEntity updateUser(@RequestBody String userInfo) {
		try {
			// getting userDto from json
			UserDto user = getUserDtoFromJson(userInfo);

			// getting password
			String password = mapper.readValue(userInfo, ObjectNode.class).get("password").asText();

			userService.updateUser(user, password, getCurrentUserEmail());
			return new ResponseEntity<>("", HttpStatus.OK);
		} catch (SnapdirectorException e) {
			return new ResponseEntity<>(e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
		} catch (IOException e) {
			return new ResponseEntity<>("Failed to parse json", HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}


	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity getAllUsers() {
		try {
			return new ResponseEntity<>(userService.getAllUsers(), HttpStatus.OK);
		} catch (DataAccessException e) {
			return new ResponseEntity<>(Collections.emptyList(), HttpStatus.NOT_ACCEPTABLE);
		}
	}


	@RequestMapping(value = "/{userEmail:.+}", method = RequestMethod.DELETE)
	public ResponseEntity removeUser(@PathVariable("userEmail") String userEmail) {
		try {
			userService.removeUser(userEmail, getCurrentUserEmail());
			return new ResponseEntity<>("", HttpStatus.OK);
		} catch (SnapdirectorException e) {
			return new ResponseEntity<>(e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}


	private String getCurrentUserEmail() {
		String session = servletRequest.getSession().getId();
		return ((Map<String, String>) context.getAttribute(Constants.CONTEXT_ALLOWED_SESSIONS_ATR_NAME)).get(session);
	}

	private UserDto getUserDtoFromJson(String json) throws IOException {
		if (mapper == null) {
			mapper = new ObjectMapper();
			mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		}
		return mapper.readValue(json, UserDto.class);
	}
}
