package com.sungardas.snapdirector.aws.dynamodb.model;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBIgnore;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;
import com.amazonaws.util.json.Jackson;

import java.util.LinkedHashMap;
import java.util.Map;


@DynamoDBTable(tableName = "Users")
public class User {

    private final Map<String, Object> attributes = new LinkedHashMap<>();
    public User() {
        super();
    }

    public User(String firstName, String lastName, String email, String password, String role) {
        this.setFirstName(firstName);
        this.setLastName(lastName);
        this.setEmail(email);
        this.setPassword(password);
        this.setRole(role);
    }

    @DynamoDBAttribute(attributeName = "firstName")
    public String getFirstName() {
        return (String) attributes.get("firstName");
    }

    public void setFirstName(String firstName) {
        attributes.put("firstName", firstName);
    }


    @DynamoDBAttribute(attributeName = "password")
    public String getPassword() {
        return (String) attributes.get("password");
    }

    public void setPassword(String password) {
        attributes.put("password", password);
    }

    @DynamoDBHashKey()
    public String getEmail() {
        return (String) attributes.get("email");
    }

    public void setEmail(String email) {
        attributes.put("email", email);
    }

    @DynamoDBAttribute(attributeName = "lastName")
    public String getLastName() {
        return (String) attributes.get("lastName");
    }

    public void setLastName(String lastName) {
        attributes.put("lastName", lastName);
    }

    @DynamoDBAttribute(attributeName = "role")
    public String getRole() {
        return (String) attributes.get("role");
    }

    public void setRole(String role) {
        attributes.put("role", role);
    }

    @Override
    public String toString() {
        return getUserInfo();
    }

    @DynamoDBIgnore
    public String getUserInfo() {
        Map<String, Object> passwordLessUserInfo = new LinkedHashMap<>();
        passwordLessUserInfo.putAll(attributes);
        passwordLessUserInfo.remove("password");
        return Jackson.toJsonString(passwordLessUserInfo);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        User user = (User) o;

        if (getFirstName() != null ? !getFirstName().equals(user.getFirstName()) : user.getFirstName() != null)
            return false;
        if (getLastName() != null ? !getLastName().equals(user.getLastName()) : user.getLastName() != null)
            return false;
        if (getEmail() != null ? !getEmail().equals(user.getEmail()) : user.getEmail() != null) return false;
        if (getPassword() != null ? !getPassword().equals(user.getPassword()) : user.getPassword() != null)
            return false;
        return !(getRole() != null ? !getRole().equals(user.getRole()) : user.getRole() != null);

    }

    @Override
    public int hashCode() {
        int result = getFirstName() != null ? getFirstName().hashCode() : 0;
        result = 31 * result + (getLastName() != null ? getLastName().hashCode() : 0);
        result = 31 * result + (getEmail() != null ? getEmail().hashCode() : 0);
        result = 31 * result + (getPassword() != null ? getPassword().hashCode() : 0);
        result = 31 * result + (getRole() != null ? getRole().hashCode() : 0);
        return result;
    }
}
