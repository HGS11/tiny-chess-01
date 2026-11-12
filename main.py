import os,json,urllib.request as w
V=[0,100,320,330,500,900,0];N=[-33,-31,-18,-14,14,18,31,33];G=[-17,-16,-15,-1,1,15,16,17];D=[-17,-15,15,17];S=[-16,-1,1,16];C={0:2,7:1,112:8,119:4};ND=[0]
def parse(f):
 p=f.split();s=''.join(ch*int(ch) if ch.isdigit() else ch for ch in p[0] if ch!='/');b=[0]*128
 for i,ch in enumerate(s):
  if not ch.isdigit():b[(7-i//8)*16+i%8]=('PNBRQK'.index(ch.upper())+1)*(1,-1)[ch>'Z']
 g=sum(1<<i for i,ch in enumerate('KQkq') if len(p)>2 and ch in p[2])
 e=ord(p[3][0])-97+16*(int(p[3][1])-1) if len(p)>3 and p[3]!='-' else -1
 return b,1 if p[1]=='w' else -1,g,e
def att(b,s,t):
 for ds,pc in (((-17*t,-15*t),1),(N,2),(G,6)):
  for d in ds:
   q=s+d
   if not q&0x88 and b[q]==pc*t:return 1
 for d in G:
  q=s+d
  while not q&0x88:
   v=b[q]
   if v:
    if v*t>0 and (abs(v)==5 or abs(v)==3+(d not in D)):return 1
    break
   q+=d
 return 0
def mk(b,m,t,g,e):
 f,u,pr=m;nb=b[:];p=nb[f];a=abs(p);nb[f]=0
 if a==1 and u==e and not b[u]:nb[u-16*t]=0
 nb[u]=pr*t if pr else p
 if a==6 and abs(u-f)==2:
  r=f+3 if u>f else f-4;nb[(f+u)//2]=nb[r];nb[r]=0
 ng=g&~(3<<2*(t<0)) if a==6 else g;ng&=~C.get(f,0);ng&=~C.get(u,0)
 return nb,ng,(f+u)//2 if a==1 and abs(u-f)==32 else -1
def gen(b,t,g,e,cap=0):
 ms=[]
 for s in range(128):
  p=b[s]
  if s&0x88 or p*t<=0:continue
  a=abs(p);r=s>>4
  if a==1:
   d=16*t;u=s+d;pr=r==6-5*(t<0);Q=(5,4,3,2) if pr else (0,)
   if not u&0x88 and not b[u]:
    for q in Q:ms.append((s,u,q))
    w=s+2*d
    if not cap and r==1+5*(t<0) and not b[w]:ms.append((s,w,0))
   for dd in (d-1,d+1):
    u=s+dd
    if not u&0x88 and (b[u]*t<0 and abs(b[u])<6 or u==e):
     for q in Q:ms.append((s,u,q))
  elif a in (2,6):
   for d in N if a==2 else G:
    u=s+d
    if not u&0x88 and (b[u]*t<0 and abs(b[u])<6 or not cap and b[u]==0):ms.append((s,u,0))
   if a==6 and not cap and s==4+112*(t<0):
    k=s;B=1<<2*(t<0)
    if g&B and not b[k+1] and not b[k+2] and all(not att(b,k+i,-t) for i in (0,1,2)):ms.append((k,k+2,0))
    if g&2*B and not b[k-1] and not b[k-2] and not b[k-3] and all(not att(b,k-i,-t) for i in (0,1,2)):ms.append((k,k-2,0))
  else:
   for d in D if a==3 else S if a==4 else G:
    u=s+d
    while not u&0x88 and not b[u]:
     if not cap:ms.append((s,u,0))
     u+=d
    if not u&0x88 and b[u]*t<0 and abs(b[u])<6:ms.append((s,u,0))
 return [m for m in ms if not att((nb:=mk(b,m,t,g,e)[0]),nb.index(6*t),-t)]
def ev(b,t):
 s=0
 for q,p in enumerate(b):
  if q&0x88 or not p:continue
  a=abs(p);fl=q&7;rk=q>>4;r=rk if p>0 else 7-rk;cn=6-abs(fl-3)-abs(rk-3);v=V[a]+[0,r*9,cn*4,cn*2,0,0,12-cn*2][a]
  s+=v if p>0 else -v
 return s*t
def order(b,ms):
 return sorted(ms,key=lambda m:(-10*V[abs(b[m[1]])]+V[abs(b[m[0]])]//10-m[2]*100,m))
def sr(b,t,g,e,d,al,be):
 ND[0]+=1
 if ND[0]>=26000:return ev(b,t)
 if d<=0:
  st=ev(b,t)
  if st>=be:return be
  al=max(al,st)
  if d<-10:return al
  ms=gen(b,t,g,e,1)
 else:ms=gen(b,t,g,e)
 if not ms:return -100000 if d>0 and att(b,b.index(6*t),-t) else 0 if d>0 else al
 bs=-10**9
 for m in order(b,ms):
  nb,ng,ne=mk(b,m,t,g,e);sc=-sr(nb,-t,ng,ne,d-1,-be,-al);bs=max(bs,sc);al=max(al,sc)
  if al>=be:break
 return bs
def best(b,t,g,e):
 ms=gen(b,t,g,e)
 if not ms:return None
 ms=order(b,ms);bm=ms[0]
 for d in range(1,7):
  ND[0]=0;al=-10**9;cm=bm
  for m in ms:
   nb,ng,ne=mk(b,m,t,g,e);sc=-sr(nb,-t,ng,ne,d-1,-10**9,-al)
   if sc>al:al=sc;cm=m
  if ND[0]>=26000:break
  bm=cm;ms=[bm]+[m for m in ms if m!=bm]
 return bm
def uci(m):return ''.join(chr(97+(s&7))+str(s//16+1) for s in m[:2])+('nbrq'[m[2]-2] if m[2] else '')
def http(url,da):
 rq=w.Request(url,data=json.dumps(da).encode(),headers={'Content-Type':'application/json'})
 return json.loads(w.urlopen(rq,timeout=9).read() or b'{}')
def main():
 u=os.environ.get('MAKEFUN_ENV_URL','').rstrip('/')
 if not u:
  print(uci(best(*parse(input()))));return
 while 1:
  g=http(u+'/game',{})
  if g.get('status')=='done':break
  while g.get('status')=='your_move':
   m=best(*parse(g['fen']))
   if not m:break
   g=http(u+'/move',{'game':g['game'],'move':uci(m)})
main()
