import unittest
from memory import Memory, MEMORY_SIZE

class TestMemory(unittest.TestCase):

    def setUp(self):
        self.memory = Memory()

    def test_initial_memory_state(self):
        # Test that all memory locations are initialized to 0x00
        for address in range(MEMORY_SIZE):
            self.assertEqual(self.memory.read_byte(address), 0x00)

    def test_write_and_read_byte(self):
        # Write a byte and read it back to check the value
        test_address = 0x1234
        test_value = 0xAB
        self.memory.write_byte(test_address, test_value)
        self.assertEqual(self.memory.read_byte(test_address), test_value)

    def test_memory_boundaries(self):
        # Test reading/writing at the first and last memory addresses
        self.memory.write_byte(0x0000, 0x12)
        self.assertEqual(self.memory.read_byte(0x0000), 0x12)

        self.memory.write_byte(MEMORY_SIZE - 1, 0x34)
        self.assertEqual(self.memory.read_byte(MEMORY_SIZE - 1), 0x34)

    def test_out_of_bounds_read(self):
        # Test that reading out of bounds raises an IndexError
        with self.assertRaises(IndexError):
            self.memory.read_byte(MEMORY_SIZE)

    def test_out_of_bounds_write(self):
        # Test that writing out of bounds raises an IndexError
        with self.assertRaises(IndexError):
            self.memory.write_byte(MEMORY_SIZE, 0x12)

    def test_byte_value_limits(self):
        # Test writing values at the boundary of valid byte range (0x00 to 0xFF)
        test_address = 0x0100
        self.memory.write_byte(test_address, 0x00)
        self.assertEqual(self.memory.read_byte(test_address), 0x00)

        self.memory.write_byte(test_address, 0xFF)
        self.assertEqual(self.memory.read_byte(test_address), 0xFF)

if __name__ == '__main__':
    unittest.main()
