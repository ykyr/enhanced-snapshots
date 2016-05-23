package com.sungardas.enhancedsnapshots.service.upgrade;




public class UpgradeSystemTo003 extends UpgradeSystemTo002 {

    private static final String currentInitVersion = "0.0.2";

    /**
     * This is an example for future update logic
     */
    @Override
    public void upgrade(String tempFolder, String initVersion) {
        if (stringVersionToInt(initVersion) < stringVersionToInt(currentInitVersion)) {
            super.upgrade(tempFolder, initVersion);
        }
        // further upgrade logic

    }



}
