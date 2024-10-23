from memory import Memory

SCREEN_WIDTH = 160
SCREEN_HEIGHT = 144

class PPU:
    def __init__(self, memory: Memory):
        self.memory = memory  # Access to video memory
        self.screen = [[0] * SCREEN_WIDTH for _ in range(SCREEN_HEIGHT)]  # 160x144 screen

    def step(self):
        # Implement PPU cycle (line scanning, V-Blank, etc.)
        pass

    def render_screen(self):
        # Render screen based on video memory
        pass