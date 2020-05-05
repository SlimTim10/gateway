#include "hal.h"

#include <ArduinoProps.h>
#include <ArduinoProps_RF69.h>

#include <Arduino.h>

#include <functional.h>
#include <ReliableSerial.h>

static Radio radio(RF_CS, RF_G0, RF_RST);
static uint8_t packet[SERIAL_PACKET_MAX_LENGTH];

void setup(void) {
	Serial.begin(115200);
	while (!Serial);
	Serial.println("Initializing converter");

	enum radio_errno radio_err = initializeRadio(&radio);
	if (radio_err != RADIO_ERRNO__SUCCESS) {
		Serial.print("Radio initialization failed. Error code: "); Serial.println(radio_err);
		die();
	}

	Serial.println("READY");
}

void loop(void) {
	if (radio.available()) {
		handleRadio();
	}

	if (Serial.available() > 0) {
		handleSerial();
	}
}

static inline void handleRadio(void) {
	uint8_t length = sizeof(packet);
	
	if (radio.recv(packet, &length)) {
		if (length > PACKET_MAX_LENGTH) return;
		serialSendPacket(packet, length);
	}
}

static inline void handleSerial(void) {
	uint8_t length = sizeof(packet);

	if (serialRecvPacket(packet, &length)) {
		delay(50);
		sendPacket(&radio, packet, length);
	}
}
