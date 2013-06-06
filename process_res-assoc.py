#!/usr/bin/python

from math import log

strategies = ["lru"]#, "fifo"]#, "plru"]
domains = ["Set"]#,"IV"]
csizes = [8192,16384,32768,65536,131072,262144,3145728] #[4096,
progs = ["aes_nosched", "aes_nosched_preloading"]

associativities = [8] #1,2,4]#,8]

c_acc = {}
c_accd = {}
c_tr = {}
c_time = {}
#c_exec_time = {}
for strategy in strategies:
  c_acc.setdefault(strategy,{})
  c_accd.setdefault(strategy,{})
  c_tr.setdefault(strategy,{})
  c_time.setdefault(strategy,{})
  #c_exec_time.setdefault(strategy,{})
  for domain in domains:
    c_acc[strategy].setdefault(domain,{})
    c_accd[strategy].setdefault(domain,{})
    c_tr[strategy].setdefault(domain,{})
    c_time[strategy].setdefault(domain,{})
    #c_exec_time[strategy].setdefault(domain,{})
    for csize in csizes:
      c_acc[strategy][domain].setdefault(csize,{})
      c_accd[strategy][domain].setdefault(csize,{})
      c_tr[strategy][domain].setdefault(csize,{})
      c_time[strategy][domain].setdefault(csize,{})
      #c_exec_time[strategy][domain].setdefault(csize,{})
      for p in progs:
        c_acc[strategy][domain][csize].setdefault(p,{})
        c_accd[strategy][domain][csize].setdefault(p,{})
        c_tr[strategy][domain][csize].setdefault(p,{})
        c_time[strategy][domain][csize].setdefault(p,{})
        #c_exec_time[strategy][domain][csize].setdefault(p,{})
        for assoc in associativities:
	  c_acc[strategy][domain][csize][p].setdefault(assoc,{})
	  c_accd[strategy][domain][csize][p].setdefault(assoc,{})
	  c_tr[strategy][domain][csize][p].setdefault(assoc,{})
	  c_time[strategy][domain][csize][p].setdefault(assoc,{})
	  ifname = "%s-%s-%s-%d" % (p,strategy,domain,csize)
	  if assoc != 4:
	    ifname += "-%d-way" % (assoc)
	  f = open(ifname,"r")
	  lines = f.readlines()
	  f.close()
	  for i in range(len(lines) - 10,len(lines)):
	    line = lines[i]
	    if line.startswith("Number of valid cache configurations :"):
	      acc = float(line.split()[-2]) if domain != "plru" else -1.0
	      c_acc[strategy][domain][csize][p][assoc] = acc
	    elif line.startswith("      that is"):
	      accd = float(line.split()[-2]) if domain != "plru" else -1.0
	      c_accd[strategy][domain][csize][p][assoc] = accd
	    elif line.startswith(" # traces:"):
	      c_tr[strategy][domain][csize][p][assoc] = log(int(line.split()[-1]),2)
	    elif line.startswith(" times ("):
	      c_time[strategy][domain][csize][p][assoc] = float(line.split()[-2])
	    #elif line.startswith("Analysis took"):
	      #c_exec_time[strategy][domain][csize][p] = int(line.split()[-2])

ofile_acc = open("acc-assoc.data","w")
ofile_accd = open("accd-assoc.data","w")
ofile_tr = open("tr-assoc.data","w")
ofile_time = open("time-assoc.data","w")
ofiles = [ofile_acc,ofile_accd,ofile_tr,ofile_time]
for of in ofiles:
  of.write("\t" + "KB\t".join([str(s/1024) for s in csizes]))
  of.write("KB\n")
for strategy in strategies:
  #for of in ofiles:
    #of.write(strategy.upper() + "\n")
  for p in progs:
    for of in ofiles:
      of.write(p + "\n")
    for domain in domains:
      #for of in ofiles:
        #of.write(domain + "\t")
      for assoc in associativities:
	for of in ofiles:
	  of.write(str(assoc) + "-way\t")
	for csize in csizes:
	  ofile_acc.write("%.3f\t" % c_acc[strategy][domain][csize][p][assoc])
	  ofile_accd.write("%.3f\t" % c_accd[strategy][domain][csize][p][assoc])
	  ofile_tr.write("%.3f\t" % c_tr[strategy][domain][csize][p][assoc])
	  ofile_time.write("%.3f\t" % c_time[strategy][domain][csize][p][assoc])
	for of in ofiles:
	  of.write("\n")
	  #of.write("\nexecution time (sec)\t%d\n" % c_exec_time[strategy][domain][csize][p])

for of in ofiles:
  of.close()
        
        
