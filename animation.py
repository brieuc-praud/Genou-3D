import matplotlib.pyplot as plt
import matplotlib.animation as animation

fichier_entree = "quat.dat"

xmin = -150
xmax = 150
ymin = -30
ymax = 140
zmin = -20
zmax = 150

fig = plt.figure("Articulation du genou")
ax = fig.add_subplot(111, projection="3d")  # Affichage en 3D

ax.set_xlim(xmin, xmax)
ax.set_ylim(ymin, ymax)
ax.set_zlim(zmin, zmax)
ax.set_xlabel("x")
ax.set_ylabel("y")
ax.set_zlabel("z")


class barre:
    def __init__(self, a1, a2):
        (self._ligne,) = ax.plot([], [], [], color=(0.0, 0.0, 1.0), linewidth=5)
        self._a1 = a1
        self._a2 = a2

    def _longueur(self, i):
        return (
            (self._a2[0][i] - self._a1[0][i]) ** 2
            + (self._a2[1][i] - self._a2[1][i]) ** 2
            + (self._a2[2][i] - self._a1[2][i]) ** 2
        ) ** 0.5

    def tracer(self, i):
        self._ligne.set_data_3d(
            [self._a1[0][i], self._a2[0][i]],
            [self._a1[1][i], self._a2[1][i]],
            [self._a1[2][i], self._a2[2][i]],
        )


def animer(i):
    i = i % n  # Pour boucler l'animation plutôt que de planter

    # Tracer le modèle

    femur.tracer(i)
    tibia.tracer(i)

    med_artcont.tracer(i)
    lat_artcont.tracer(i)

    LCA.tracer(i)
    LCP.tracer(i)
    LCM.tracer(i)


# lecture du fichier de donnees
f1 = list()
f2 = list()
f3 = list()
f4 = list()
f5 = list()
t1 = list()
t2 = list()
t3 = list()
t4 = list()
t5 = list()
with open(fichier_entree, "r") as fichier:
    for i in range(3):  # crée des listes vides
        t1.append(list())
        t2.append(list())
        t3.append(list())
        t4.append(list())
        t5.append(list())
        f1.append(list())
        f2.append(list())
        f3.append(list())
        f4.append(list())
        f5.append(list())
    for ligne in fichier:
        donnees = ligne.split()
        for k in range(3):
            t1[k].append(float(donnees[0 + k]))
            t2[k].append(float(donnees[3 + k]))
            t3[k].append(float(donnees[6 + k]))
            t4[k].append(float(donnees[9 + k]))
            t5[k].append(float(donnees[12 + k]))
            f1[k].append(float(donnees[15 + k]))
            f2[k].append(float(donnees[18 + k]))
            f3[k].append(float(donnees[21 + k]))
            f4[k].append(float(donnees[24 + k]))
            f5[k].append(float(donnees[27 + k]))


n = len(f1[0])  # = len(t1[0]) = len(f2[0]) = ...

# Les 5 barres
tibia = barre(t3, t4)
femur = barre(f3, f4)

LCA = barre(t3, f3)
LCP = barre(t4, f4)
LCM = barre(t5, f5)
med_artcont = barre(f2, t2)
lat_artcont = barre(f1, t1)

anim = animation.FuncAnimation(
    fig, animer, frames=n, interval=1, repeat=True
)

plt.show()
