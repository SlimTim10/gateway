#include "hal.h"

#include <Puzzles.h>

#include <ArduinoProps.h>
#include <ArduinoProps_config.h>
#include <ArduinoProps_RF69.h>

#include <Arduino.h>

static Radio radio(RF_CS, RF_G0, RF_RST);

void setup(void) {
	Serial.begin(115200);
	while (!Serial);
	Serial.println("Initializing receiver");

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


static inline void waitForSerialData(void) {
	delay(100);
}

static inline void handleRadio(void) {
	uint8_t packet[PACKET_MAX_LENGTH];
	uint8_t len = sizeof(packet);
	if (radio.recv(packet, &len)) {
		Serial.println((char *) packet);
		Serial.flush();
	}
}

static inline void handleSerial(void) {
	waitForSerialData();

	uint8_t packet[PACKET_MAX_LENGTH];
	size_t len;
	for (len = 0; Serial.available() > 0 && len < PACKET_MAX_LENGTH; len++) {
		packet[len] = Serial.read();
	}
	sendPacket(&radio, packet, len);
}
