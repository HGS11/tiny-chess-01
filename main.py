import os,json,urllib.request as w
V=[0,100,320,330,500,900,900];N=[-33,-31,-18,-14,14,18,31,33];G=[-17,-16,-15,-1,1,15,16,17];D=[-17,-15,15,17];S=[-16,-1,1,16];C={0:2,7:1,112:8,119:4};ND=[0];TT={};KL={}
class A(Exception):pass
def parse(f):
 p=f.split();b=[0]*128;i=112
 for ch in p[0]:
  if ch=='/':i-=24
  elif ch<'A':i+=int(ch)
  else:b[i]=('PNBRQK'.index(ch.upper())+1)*(1,-1)[ch>'Z'];i+=1
 g=sum((c in p[2])<<i for i,c in enumerate('KQkq'))
 e=ord(p[3][0])+16*int(p[3][1])-113 if p[3]!='-'else-1
 return b,1-2*(p[1]=='b'),g,e
def att(b,s,t):
 for ds,pc in(((-17*t,-15*t),1),(N,2),(G,6)):
  for d in ds:
   if(s+d)&136<1 and b[s+d]==pc*t:return 1
 for d in G:
  q=s
  while(q:=q+d)&136<1:
   v=b[q]
   if v:
    if v*t>0 and abs(v)in(5,3+(d in S)):return 1
    break
 return 0
def mk(b,m,t,g,e):
 f,u,pr=m;nb=b[:];a=abs(p:=nb[f]);nb[f]=0
 if a==1 and u==e:nb[u-16*t]=0
 nb[u]=pr*t or p
 if a==6 and abs(u-f)==2:
  r=f+(3,-4)[u<f];nb[(f+u)//2]=nb[r];nb[r]=0
 ng=g&~((3,12)[t<0]*(a>5)|C.get(f,0)|C.get(u,0))
 return nb,ng,(f+u)//2 if a==1 and u-f==32*t else-1
def gen(b,t,g,e,cap=0):
 ms=[];P=ms.append
 for s,p in enumerate(b):
  if s&136 or p*t<=0:continue
  a=abs(p);r=s>>4
  if a==1:
   d=16*t;u=s+d;pr=r==(6,1)[t<0];Q=(5,4,3,2)if pr else(0,)
   if u&136<1 and not b[u]:
    for q in Q:P((s,u,q))
    w=s+2*d
    if not cap and r==(1,6)[t<0] and not b[w]:P((s,w,0))
   for dd in(-1,1):
    u=s+d+dd
    if u&136<1 and(-6<b[u]*t<0 or u==e):
     for q in Q:P((s,u,q))
  elif a in(2,6):
   for d in(N,G)[a==6]:
    u=s+d
    if u&136<1 and(-6<b[u]*t<0 or not cap and not b[u]):P((s,u,0))
   if a==6 and not cap and s==4+112*(t<0):
    for B,o in((1,1),(2,-1)):
     if g&B<<2*(t<0)and not b[s+o]and not b[s+2*o]and(o>0 or not b[s+3*o])and all(not att(b,s+i*o,-t)for i in(0,1,2)):P((s,s+2*o,0))
  else:
   for d in(D,S,G)[a-3]:
    u=s+d
    while u&136<1 and not b[u]:P((s,u,0))if not cap else 0;u+=d
    if u&136<1 and-6<b[u]*t<0:P((s,u,0))
 k=b.index(6*t)
 return[m for m in ms if not att((nb:=mk(b,m,t,g,e)[0]),m[1]if b[m[0]]in(6,-6)else k,-t)]
def ev(b,t):
 s=0;ph=0;kc=0
 for q,p in enumerate(b):
  if q&136 or not p:continue
  a=abs(p);sg=p>0 or-1;fl,rk=q&7,q>>4;r=(rk,7-rk)[sg<0];cn=6-abs(fl-3)-abs(rk-3);v=V[a]+[0,r*9+(r>4)*30,cn*4,cn*2,(r==6)*20,0,0][a]
  ph+=V[a]*(1<a<6);kc+=cn*sg*(a>5);s+=v*sg
 s+=kc*(8-12*(ph>=1600))
 return s*t
def order(b,ms,hm,k):
 return sorted(ms,key=lambda m:(-(m==hm)*999-(m==k)*30-10*abs(b[m[1]])+abs(b[m[0]])-m[2]*100))
def sr(b,t,g,e,d,al,be):
 ND[0]+=1
 if ND[0]>=4e4:raise A
 hm=0;a0=al;k=b.index(6*t)
 if d>0 and(z:=TT.get(ky:=(tuple(b),t,g,e))):
  hm=z[3]
  if z[0]>=d:
   if z[2]==0:return z[1]
   if z[2]>0:al=max(al,z[1])
   else:be=min(be,z[1])
   if al>=be:return z[1]
 if d<=0 and(d<-8 or not att(b,k,-t)):
  al=max(al,ev(b,t))
  if al>=be or d<-4:return al
  ms=[m for m in gen(b,t,g,e,1)if m[2]or abs(b[m[1]])>=abs(b[m[0]])]
 else:ms=gen(b,t,g,e)
 if not ms:return -1e5 if att(b,k,-t)else(0,al)[d<=0]
 bm=0
 for i,m in enumerate(order(b,ms,hm,KL.get(d,0))):
  nb,ng,ne=mk(b,m,t,g,e);r=d>2<i and b[m[1]]==m[2]==0;S=lambda D:-sr(nb,-t,ng,ne,D,-be,-al)
  sc=S(d-1-r)
  if r and sc>al:sc=S(d-1)
  if sc>al:al=sc;bm=m
  if al>=be:break
 if d>0:
  if al>=be and bm and b[bm[1]]==bm[2]==0:KL[d]=bm
  if len(TT)>2e4:TT.clear()
  TT[ky]=(d,al,(al>=be)-(al<=a0),bm)
 return al
def best(b,t,g,e):
 ms=gen(b,t,g,e)
 if not ms:return None
 bm=ms[0]
 for d in range(1,7):
  ND[0]=0;al=-1e9;cm=bm
  try:
   for m in order(b,ms,bm,0):
    nb,ng,ne=mk(b,m,t,g,e);sc=-sr(nb,-t,ng,ne,d-1,-1e9,-al)
    if sc>al:al=sc;cm=m
  except A:break
  bm=cm
  if al>9e4:break
 return bm
def uci(m):return ''.join(chr(97+(s&7))+str(s//16+1) for s in m[:2])+('nbrq'[m[2]-2]if m[2]else'')
def http(u,d):return json.loads(w.urlopen(w.Request(u,json.dumps(d).encode()),timeout=9).read()or b'{}')
def main():
 u=os.getenv('MAKEFUN_ENV_URL','').rstrip('/')
 while 1:
  g=http(u+'/game',{})
  if g.get('status')=='done':break
  while g.get('status')=='your_move':
   m=best(*parse(g['fen']))
   if not m:break
   g=http(u+'/move',{'game':g['game'],'move':uci(m)})
main()
