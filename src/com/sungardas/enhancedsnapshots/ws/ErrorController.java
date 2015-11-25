package com.sungardas.enhancedsnapshots.ws;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.SendTo;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Controller;

@Controller
public class ErrorController {


    @Autowired
    private SimpMessagingTemplate template;

    @MessageMapping("/error")
    @SendTo("/error")
    public ErrorDto greeting() throws Exception {
        return new ErrorDto("Some error message");
    }

    public class ErrorDto {

        private String content;

        public ErrorDto(String content) {
            this.content = content;
        }

        public String getContent() {
            return content;
        }

        public void setContent(String content) {
            this.content = content;
        }
    }



}
