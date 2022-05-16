# Script Blender pour animer le genou

import bpy
import os

fichier_donnees = "u.dat"

path_fichier = os.path.join(os.path.dirname(bpy.data.filepath), fichier_donnees)

x = []
y = []
z = []
theta = []
phi = []
psi = []
positions = [[0,0,0]]
angles = [[0,0,0]]
with open(path_fichier, "r") as fichier:
    for ligne in fichier:
        donnees = ligne.split() #On split la première ligne du fichier
        x.append( float(donnees[0]) )
        y.append( float(donnees[1]) )
        z.append( float(donnees[2]) )
        theta.append( float(donnees[3]))
        phi.append( float(donnees[4]))
        psi.append( float(donnees[5]))
        positions.append(  [ x[-1], y[-1], z[-1] ]  )
        angles.append(  [ theta[-1], phi[-1], psi[-1] ]  )


ob = bpy.data.objects["right_femur"]

print(len(x))
frame_num = 0
for i in range(len(positions)):
    bpy.context.scene.frame_set(frame_num)

    ob.location = positions[i]
    ob.rotation_euler = angles[i]

    ob.keyframe_insert(data_path="location", index=-1)
    ob.keyframe_insert(data_path="rotation_euler", index=-1)

    frame_num += 5