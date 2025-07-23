
## To run right click .py file under solution explorer and click "Set as startup file" --otherwise visual studio will not run
import sys
import time
import os
sys.path.append("./Functions/") # Add Function and modules
print("HELLO YOU")
print("Current directory:", os.getcwd())




# import sys
# import time

# start_time = time.time()

# sys.path.append("./Functions/") # Add Function and modules
# from TestFunctions import Test_Function
# print("HELLO")
# Test_Function()
# end_time = time.time()

# print(f"Elapsed time: {end_time - start_time:.4f} seconds")
# input("Press key to proceed")