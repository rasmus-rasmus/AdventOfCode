import re

def parse_and_sum_corrupted_memory(memory):
    # Regular expressions for the valid instructions
    mul_pattern = re.compile(r'mul\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*\)')
    do_pattern = re.compile(r'do\(\)')
    dont_pattern = re.compile(r"don't\(\)")

    enabled = True  # Multiplications are enabled at the start
    total_sum = 0

    # Split the memory into chunks by instructions or special characters
    chunks = re.split(r'([^a-zA-Z0-9()\s])', memory)
    for chunk in chunks:
        chunk = chunk.strip()
        
        # Check for do() and don't() instructions
        if do_pattern.match(chunk):
            enabled = True
        elif dont_pattern.match(chunk):
            enabled = False
        
        # Check for valid mul(X, Y) instructions
        elif enabled:
            match = mul_pattern.match(chunk)
            if match:
                x, y = int(match.group(1)), int(match.group(2))
                total_sum += x * y

    return total_sum

# Example corrupted memory string
corrupted_memory = "do()xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

# Calculate the sum of the results of the enabled multiplications
result = parse_and_sum_corrupted_memory(corrupted_memory)
print(f"Sum of the enabled multiplications: {result}")
