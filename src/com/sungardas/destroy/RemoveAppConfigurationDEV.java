package com.sungardas.destroy;

import javax.annotation.PostConstruct;

public class RemoveAppConfigurationDEV {

    @PostConstruct
    private void init() {
        System.out.println("Destroy complete");
    }
}
