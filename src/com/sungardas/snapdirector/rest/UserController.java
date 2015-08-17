package com.sungardas.snapdirector.rest;

import com.sungardas.snapdirector.dto.UserDto;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.exception.UniqueConstraintViolationException;
import com.sungardas.snapdirector.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;

import static org.springframework.web.bind.annotation.RequestMethod.GET;


@RestController
@RequestMapping("/user")
public class UserController {


    @Autowired
    private UserService userService;

    @RequestMapping(method = RequestMethod.POST)
    public ResponseEntity<String> createUser(@ModelAttribute UserDto userInfo, @RequestParam String password) {
        ResponseEntity<String> responseEntity;
        try {
            userService.createUser(userInfo, password);
            responseEntity = new ResponseEntity<>("User was successfully created.", HttpStatus.OK);
        } catch (SnapdirectorException e) {
            responseEntity = new ResponseEntity<>(e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return responseEntity;
    }


    @RequestMapping(method = GET)
    public ResponseEntity getAllUsers(){
        try {
            return new ResponseEntity<>(userService.getAllUsers(), HttpStatus.OK);
        } catch (DataAccessException e){
            return new ResponseEntity<>(Collections.emptyList(), HttpStatus.NOT_ACCEPTABLE);
        }
    }
}
