#!/usr/bin/python

from math import log

strategies = ["lru", "fifo", "plru"]
domains = ["Set","IV"]
csizes = [4096,8192,16384,32768,65536,131072,262144,3145728]
progs = ["aes_nosched", "aes_nosched_preloading"]

c_acc = {}
c_accd = {}
c_tr = {}
c_time = {}
c_exec_time = {}
for strategy in strategies:
  c_acc.setdefault(strategy,{})
  c_accd.setdefault(strategy,{})
  c_tr.setdefault(strategy,{})
  c_time.setdefault(strategy,{})
  c_exec_time.setdefault(strategy,{})
  for domain in domains:
    c_acc[strategy].setdefault(domain,{})
    c_accd[strategy].setdefault(domain,{})
    c_tr[strategy].setdefault(domain,{})
    c_time[strategy].setdefault(domain,{})
    c_exec_time[strategy].setdefault(domain,{})
    for csize in csizes:
      c_acc[strategy][domain].setdefault(csize,{})
      c_accd[strategy][domain].setdefault(csize,{})
      c_tr[strategy][domain].setdefault(csize,{})
      c_time[strategy][domain].setdefault(csize,{})
      c_exec_time[strategy][domain].setdefault(csize,{})
      for p in progs:
        c_acc[strategy][domain][csize].setdefault(p,{})
        c_accd[strategy][domain][csize].setdefault(p,{})
        c_tr[strategy][domain][csize].setdefault(p,{})
        c_time[strategy][domain][csize].setdefault(p,{})
        c_exec_time[strategy][domain][csize].setdefault(p,{})
        ifname = "%s-%s-%s-%d" % (p,strategy,domain,csize)
        f = open(ifname,"r")
        lines = f.readlines()
        f.close()
        for i in range(len(lines) - 50,len(lines)):
          line = lines[i]
          if line.startswith("Number of valid cache configurations :"):
            if domain == "plru":
              print "help"
            acc = float(line.split()[-2]) if strategy != "plru" else -1
            c_acc[strategy][domain][csize][p] = acc
          elif line.startswith("      that is"):
            accd = float(line.split()[-2]) if strategy != "plru" else -1
            c_accd[strategy][domain][csize][p] = accd
          elif line.startswith(" # traces:"):
            c_tr[strategy][domain][csize][p] = log(int(line.split()[-1]),2)
          elif line.startswith(" times ("):
            c_time[strategy][domain][csize][p] = float(line.split()[-2])
          elif line.startswith("Analysis took"):
            c_exec_time[strategy][domain][csize][p] = int(line.split()[-2])

ofile_acc = open("acc.data","w")
ofile_accd = open("accd.data","w")
ofile_tr = open("tr.data","w")
ofile_time = open("time.data","w")
ofiles = [ofile_acc,ofile_accd,ofile_tr,ofile_time]
for of in ofiles:
  of.write("\t" + "KB\t".join([str(s/1024) for s in csizes]))
  of.write("KB\n")
for p in progs:
  for of in ofiles:
    of.write(p + "\n")
  for strategy in strategies:
    for of in ofiles:
      of.write(strategy.upper() + "\n")
    for domain in domains:
      for of in ofiles:
        of.write(domain + "\t")
      for csize in csizes:
        ofile_acc.write("%.3f\t" % c_acc[strategy][domain][csize][p])
        ofile_accd.write("%.3f\t" % c_accd[strategy][domain][csize][p])
        ofile_tr.write("%.3f\t" % c_tr[strategy][domain][csize][p])
        ofile_time.write("%.3f\t" % c_time[strategy][domain][csize][p])
      for of in ofiles:
        of.write("\nexecution time (sec)\t%d\n" % c_exec_time[strategy][domain][csize][p])

for of in ofiles:
  of.close()
        
        
