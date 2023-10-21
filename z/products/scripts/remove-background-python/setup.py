from rembg import remove

background_color = (251, 251, 253, 255)

input_path = '3.png'
output_path = 'output.png'

with open(input_path, 'rb') as i:
    with open(output_path, 'wb') as o:
        input = i.read()
        output = remove(input, bgcolor = background_color,  alpha_matting=True, alpha_matting_foreground_threshold=270,alpha_matting_background_threshold=20, alpha_matting_erode_size=11)
        o.write(output)