package com.sungardas.enhancedsnapshots.aws.dynamodb.repository;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.User;
import org.socialsignin.spring.data.dynamodb.repository.EnableScan;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

@EnableScan
public interface UserRepository extends CrudRepository<User, String> {
    List<User> findByRoleAndInstanceId(String lastName, String instanceId);

    List<User> findByEmailAndPasswordAndInstanceId(String email, String password, String instanceId);

    List<User> findByEmailAndInstanceId(String email, String instanceId);

    List<User> findByInstanceId(String instanceId);
}