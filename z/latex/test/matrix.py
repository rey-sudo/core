import numpy as np
import matplotlib.pyplot as plt
import json
import seaborn as sns

def visualize_data(datum):
    data = datum['array']
    num_iterations = len(data)
    num_indices = len(data[0])

    matrix = np.zeros((num_indices, num_iterations))
    for i, iteration in enumerate(data):
        for j, value in enumerate(iteration):
            matrix[j, i] = value

    # Generate the matrix plot
    plt.figure(figsize=(8, 6))
    plt.imshow(matrix, cmap='coolwarm', aspect='auto')

    plt.colorbar(label='AuditorIndex')
    plt.title('Matrix Fisher-Yates ' + str(datum['iterations']) + ' Iterations' )
    plt.xlabel('Iterations')
    plt.ylabel('ArrayIndex')



    plt.show()





with open('arrayData.json') as file:
         data = json.load(file)
         visualize_data(data)