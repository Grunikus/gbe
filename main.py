from memory import Memory
from cpu import CPU
from apu import APU
from ppu import PPU

class GameBoy:
    ROM_START_ADDRESS = 0x0100

    def __init__(self):
        self.memory = Memory()
        self.cpu = CPU(self.memory)
        self.ppu = PPU(self.memory)
        self.apu = APU()

    def load_rom(self, rom_data):
        # Load the ROM into memory starting at 0x0100
        for i, byte in enumerate(rom_data):
            self.memory.write_byte(GameBoy.ROM_START_ADDRESS + i, byte)

    def step(self):
        # Simulate one complete cycle of the Game Boy
        self.cpu.step()
        self.ppu.step()
        self.apu.step()

    def run(self):
        # Continuously run the console
        while True:
            self.step()

class ROMLoader:
    @staticmethod
    def load_from_file(filepath):
        with open(filepath, "rb") as file:
            return list(file.read())
