#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
import time

plt.ion()

#fig = plt.figure(figsize=(8,4))
wm = plt.get_current_fig_manager()
wm.window.wm_geometry("800x800+50+0")
plt.title("bla")
plt.ylabel('Scores')
plt.xlabel('obj')
numpic=0
while(True):
	time.sleep(0.05)
	try:
		with open("datatest.txt") as f:
			data = f.read()

		if len(data)>1:
			data = data.split('\n')
			plt.clf()
			plt.axis([-0.1,1.5,-0.1,1.5])
			plt.title("positions")
			plt.ylabel('Y')
			plt.xlabel('X')
			
			plt.scatter([0.6],[1.0],c='red',marker='o')
			circle=plt.Circle((0.6,1.0),0.1,color='#aabbff',fill=True, zorder=0)
			
			circle2=plt.Circle((0.5,0.8),0.2,color='r',fill=True,alpha=0.3, zorder=0)
			plt.gca().add_patch(circle2)
			plt.gca().add_patch(circle)
			data.pop()
			data.pop()
			x={}
			y={}
			z={}
			greedy={}
			color={}
			for row in data:
				id=row.split(' ')[0]
				print row
				if id in x:
					x[id].append(float(row.split(' ')[1]))
					y[id].append(float(row.split(' ')[2]))
					z[id].append(float(row.split(' ')[3]))
					greedy[id].append(row.split(' ')[7])
					color[id].append( ( float(row.split(' ')[4]),float(row.split(' ')[5]),float(row.split(' ')[6])) )
				else:		
					x[id]=[float(row.split(' ')[1])]
					y[id]=[float(row.split(' ')[2])]
					z[id]=[float(row.split(' ')[3])]
					greedy[id]=["false"]
					color[id]=[ ( float(row.split(' ')[4]),float(row.split(' ')[5]),float(row.split(' ')[6])) ]
			print x
			exit
			for idobj in x:
				plt.scatter(x[idobj],y[idobj],c=color[idobj],marker='o')
				for ind,p in enumerate(x[idobj][1:]):
					if greedy[idobj][ind+1]=="false":
						plt.plot( x[idobj][ind:ind+2],y[idobj][ind:ind+2],color=(1.0,0.5,0.0), linewidth=2)
					else:
						plt.plot( x[idobj][ind:ind+2],y[idobj][ind:ind+2],color='black', linewidth=1)

			
			#plt.plot( [0.2,0.2],[0.07,0.13],color='black', linewidth=3,alpha=0.6)
			#plt.plot( [0.15,0.15],[-0.03,0.03],color='black', linewidth=3,alpha=0.6)
			#plt.plot( [0.2,0.2],[0.0,0.2],color='black', linewidth=2,alpha=0.6)
			plt.draw()
			#numpic=numpic+1
			#plt.savefig('test'+str(numpic)+'.png', format='png', dpi=500)
	except:
		time.sleep(0.05)
		f.close()
		#print 'bad'
	#time.sleep(0.0)
	#print 'ok'
