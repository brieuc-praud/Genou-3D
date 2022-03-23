import matplotlib.pyplot as plt
import matplotlib.animation as animation

fichier_entree = "simulation.dat"
fichier_parametres = "parametres.dat" #pour lire les longueurs à vide des ligaments

couleur_traction = (1.0, 0.0, 0.0 )#rouge
couleur_compression = (0.0 , 0.0 , 1.0)#bleu
couleur_neutre = (0.0, 0.0, 1.0)#bleu

xmin = -5
xmax = 30
ymin = -5
ymax = 30
zmin = -5
zmax = 30

fig = plt.figure("Articulation du genou")
ax = fig.add_subplot(111, projection='3d')  # Affichage en 3D

ax.set_xlim(xmin, xmax)
ax.set_ylim(ymin, ymax)
ax.set_zlim(zmin, zmax)

with open(fichier_parametres, "r") as fichier:
    donnees = fichier.readline().split() #On split la première ligne du fichier
    l0_LCA = float(donnees[0])
    l0_LCP = float(donnees[2])

class barre:
    def __init__(self, a1, a2):
        self._ligne, = ax.plot([], [], [], color=couleur_neutre, linewidth=5)
        self._a1 = a1
        self._a2 = a2
    def tracer(self, i):
        self._ligne.set_data_3d( [ self._a1[0][i], self._a2[0][i] ], [ self._a1[1][i], self._a2[1][i]], [ self._a1[2][i], self._a2[2][i] ])

class ligament(barre):
    def __init__(self, a1, a2, l0):
        super().__init__(a1, a2)
        self._l0 = l0
    def _longueur(self, i):
        return ( (self._a2[0][i] - self._a1[0][i])**2 + (self._a2[1][i] - self._a2[1][i])**2 + (self._a2[2][i] - self._a1[2][i])**2 )**0.5
    def allongement(self, i):
        return self._longueur(i) - self._l0
    def tracer(self, i):
        super().tracer(i)
    def colorer(self, i, allongement_max):
        t = 0.5*( self.allongement(i) + allongement_max ) / allongement_max
        self._ligne.set_color(  tuple(map(lambda i, j: i*t + (1-t)*j, couleur_compression, couleur_traction))  )


def animer(i):
    i=i*10
    i=i%n
    #tracer le modèle
    femur.tracer(i)
    tibia.tracer(i)

    # med_artcont.tracer(i)
    # lat_artcont.tracer(i)
    # med_artcont.colorer(i, allongement_max)
    # lat_artcont.colorer(i, allongement_max)

    LCA.tracer(i)
    LCP.tracer(i)
    # LCM.tracer(i)
    LCA.colorer(i, allongement_max)
    LCP.colorer(i, allongement_max)
    # LCM.colorer(i, allongement_max)


#lecture fichier donnees
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
    for i in range(3): #crée des listes vides
        f1.append(list())
        f2.append(list())
        f3.append(list())
        f4.append(list())
        f5.append(list())
        t1.append(list())
        t2.append(list())
        t3.append(list())
        t4.append(list())
        t5.append(list())
    for ligne in fichier:
        donnees = ligne.split()
        # for k in range(3):
        #     t1[k].append(float(donnees[0+k]))
        #     f2[k].append(float(donnees[3+k]))
        #     f1[k].append(float(donnees[6+k]))
        #     t2[k].append(float(donnees[9+k]))
        #     t3[k].append(float(donnees[12+k]))
        #     f4[k].append(float(donnees[15+k]))
        #     f3[k].append(float(donnees[18+k]))
        #     t4[k].append(float(donnees[21+k]))
        #     t5[k].append(float(donnees[24+k]))
        #     f5[k].append(float(donnees[27+k]))
        for k in range(2):
            f1[k].append(float(donnees[0+k]))#B
            f2[k].append(float(donnees[2+k]))#C
            t1[k].append(float(donnees[4+k]))#A
            t2[k].append(float(donnees[6+k]))#D
        f1[2].append(float(0))#B
        f2[2].append(float(0))#C
        t1[2].append(float(0))#A
        t2[2].append(float(0))#D

n = len(f1[0]) # = len(t1[0]) = len(f2[0]) = ...

#Les 4 barres
tibia = barre(t1, t2)
femur = barre(f1, f2)

LCA = ligament(t1, f1, l0_LCA)
LCP = ligament(f2, t2, l0_LCP)
#LCM = ligament(f3,t3, l0_LCM)
#med_artcont = barre(f4,t4)
#lat_artcont = barre(f5,t5)


#On détermine ici l'allongement maximal afin de l'avoir comme référence pour définir la couleur des ligaments
allongement_max = max( abs(LCA.allongement(0)) , abs(LCP.allongement(0)) )
# allongement_max = max( abs(LCA.allongement(0)) , abs(LCP.allongement(0)), abs(LCM.allongement(0)) )
for i in range(1,n):
    allongement = max( abs(LCA.allongement(i)) , abs(LCP.allongement(i)) )
    # allongement = max( abs(LCA.allongement(i)) , abs(LCP.allongement(i)), abs(LCM.allongement(0)) )
    if allongement > allongement_max:
        allongement_max = allongement

anim = animation.FuncAnimation(fig, animer, frames=n, interval=1, repeat=True) #5000//n => l'animation se fait sur 5 secondes (à condition que 5000//n > 1)


plt.show()