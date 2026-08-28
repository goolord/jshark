{-# LANGUAGE OverloadedStrings #-}

-- | JS emit fragments for LUT create/step (main thread + worker bundle).
--
-- 'lifeLutWorkerBootJs' must match 'LutCore.stepRegionLUTPure'; see
-- @test/LifeWorkerTests.hs@. Main thread installs via 'Lut.bootLifeLut'.
module LutBoot
  ( lifeLutGlobalJs
  , lifeLutInstallJs
  , lifeLutWorkerBootJs
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Types (speciesCount)

lifeLutGlobalJs :: Text
lifeLutGlobalJs =
  "(typeof globalThis!=='undefined'?globalThis:self).__jsharkLifeLut"

lifeLutComputeNextByteJs :: Text
lifeLutComputeNextByteJs =
  "function computeNextByte(top,cur,bot,lt,lc,lb,rt,rc,rb){"
    <> "let out=0;"
    <> "for(let bit=0;bit<8;bit++){"
    <> "const alive=(cur>>bit)&1;"
    <> "const left=bit>0?(cur>>(bit-1))&1:lc;"
    <> "const right=bit<7?(cur>>(bit+1))&1:rc;"
    <> "const topL=bit>0?(top>>(bit-1))&1:lt;"
    <> "const topC=(top>>bit)&1;"
    <> "const topR=bit<7?(top>>(bit+1))&1:rt;"
    <> "const botL=bit>0?(bot>>(bit-1))&1:lb;"
    <> "const botC=(bot>>bit)&1;"
    <> "const botR=bit<7?(bot>>(bit+1))&1:rb;"
    <> "const n=topL+topC+topR+left+right+botL+botC+botR;"
    <> "const next=alive?n===2||n===3:n===3;"
    <> "if(next)out|=1<<bit;"
    <> "}"
    <> "return out;"
    <> "}"

lifeLutStepChunkJs :: Text
lifeLutStepChunkJs =
  "function stepChunk(LUT,top,cur,bot,lt,lc,lb,rt,rc,rb){"
    <> "const edge=lt|lc|lb|rt|rc|rb;"
    <> "if((top|cur|bot|edge)===0)return 0;"
    <> "if(bot===0&&edge===0)return LUT[(top<<8)|cur];"
    <> "return computeNextByte(top,cur,bot,lt,lc,lb,rt,rc,rb);"
    <> "}"

lifeLutCreateBodyJs :: Text
lifeLutCreateBodyJs =
  "const LUT=new Uint8Array(65536);"
    <> "for(let key=0;key<65536;key++){"
    <> "const top=(key>>8)&255,cur=key&255;"
    <> "let out=0;"
    <> "for(let bit=0;bit<8;bit++){"
    <> "const alive=(cur>>bit)&1;"
    <> "const left=bit>0?(cur>>(bit-1))&1:0;"
    <> "const right=bit<7?(cur>>(bit+1))&1:0;"
    <> "const topL=bit>0?(top>>(bit-1))&1:0;"
    <> "const topC=(top>>bit)&1;"
    <> "const topR=bit<7?(top>>(bit+1))&1:0;"
    <> "const n=topL+topC+topR+left+right;"
    <> "const next=alive?n===2||n===3:n===3;"
    <> "if(next)out|=1<<bit;"
    <> "}"
    <> "LUT[key]=out;"
    <> "}"
    <> "return LUT;"

lifeLutStepRegionFnJs :: Text
lifeLutStepRegionFnJs =
  "function stepRegionLUT(LUT,gridA,gridB,w,h,y0,y1){"
    <> "const yStart=Math.max(1,y0);"
    <> "const yStop=Math.min(h-1,y1);"
    <> "for(let y=yStart;y<yStop;y++){"
    <> "const topOff=(y-1)*w,curOff=y*w,botOff=(y+1)*w;"
    <> "gridB.fill(0,curOff,curOff+w);"
    <> "const bytes=((w+7)/8)|0;"
    <> "for(let xb=0;xb<bytes;xb++){"
    <> "const x0=xb*8;"
    <> "if(x0>=w)continue;"
    <> "const leftCol=x0-1,rightCol=x0+8;"
    <> "const lt=x0>0&&(gridA[topOff+leftCol]&1)?1:0;"
    <> "const lc=x0>0&&(gridA[curOff+leftCol]&1)?1:0;"
    <> "const lb=x0>0&&(gridA[botOff+leftCol]&1)?1:0;"
    <> "const rt=rightCol<w&&(gridA[topOff+rightCol]&1)?1:0;"
    <> "const rc=rightCol<w&&(gridA[curOff+rightCol]&1)?1:0;"
    <> "const rb=rightCol<w&&(gridA[botOff+rightCol]&1)?1:0;"
    <> "let top=0,cur=0,bot=0;"
    <> "for(let b=0;b<8;b++){"
    <> "const x=x0+b;"
    <> "if(x>=w)continue;"
    <> "const sh=1<<b;"
    <> "if(gridA[topOff+x]&1)top|=sh;"
    <> "if(gridA[curOff+x]&1)cur|=sh;"
    <> "if(gridA[botOff+x]&1)bot|=sh;"
    <> "}"
    <> "if((top|cur|bot|lt|lc|lb|rt|rc|rb)===0)continue;"
    <> "const next=stepChunk(LUT,top,cur,bot,lt,lc,lb,rt,rc,rb);"
    <> "for(let b=0;b<8;b++){"
    <> "const x=x0+b;"
    <> "if(x>=w)continue;"
    <> "gridB[curOff+x]=next&(1<<b)?1:0;"
    <> "}"
    <> "}"
    <> "}"
    <> "if(y0===0)gridB.set(gridA.subarray(0,w));"
    <> "if(y1>=h){const botOff=(h-1)*w;gridB.set(gridA.subarray(botOff,h*w),botOff);}"
    <> "}"

lifeLutStepCoreJs :: Text
lifeLutStepCoreJs =
  lifeLutComputeNextByteJs
    <> lifeLutStepChunkJs
    <> "function createLifeLUT(){"
    <> lifeLutCreateBodyJs
    <> "}"
    <> lifeLutStepRegionFnJs

lifeLutRefreshPackedJs :: Text
lifeLutRefreshPackedJs =
  "function refreshPackedRegion(grid,w,h,x0,y0,x1,y1){"
    <> "w=w|0;h=h|0;"
    <> "if(w<=0||h<=0||!grid)return;"
    <> "const xs=Math.max(0,Math.floor(x0)-1);"
    <> "const ys=Math.max(0,Math.floor(y0)-1);"
    <> "const xe=Math.min(w-1,Math.floor(x1)+1);"
    <> "const ye=Math.min(h-1,Math.floor(y1)+1);"
    <> "if(xs>xe||ys>ye)return;"
    <> "for(let y=ys;y<=ye;y++){"
    <> "const row=y*w;"
    <> "for(let x=xs;x<=xe;x++){"
    <> "let n=0;"
    <> "for(let dy=-1;dy<=1;dy++)for(let dx=-1;dx<=1;dx++){"
    <> "if(!dx&&!dy)continue;"
    <> "const nx=x+dx,ny=y+dy;"
    <> "if(nx<0||ny<0||nx>=w||ny>=h)continue;"
    <> "if(grid[ny*w+nx]&1)n++;"
    <> "}"
    <> "grid[row+x]=(grid[row+x]&1)+n*2;"
    <> "}"
    <> "}"
    <> "}"

lifeLutPickBirthJs :: Text
lifeLutPickBirthJs =
  "function pickBirthSpecies(alive,species,w,h,x,y,counts,touched){"
    <> "let best=0,bestSid=0,touchedLen=0;"
    <> "for(let dy=-1;dy<=1;dy++)for(let dx=-1;dx<=1;dx++){"
    <> "if(!dx&&!dy)continue;"
    <> "const nx=x+dx,ny=y+dy;"
    <> "if(nx<0||ny<0||nx>=w||ny>=h)continue;"
    <> "const ni=ny*w+nx;"
    <> "if(!(alive[ni]&1))continue;"
    <> "const sid=species[ni];"
    <> "const c=++counts[sid];"
    <> "if(c===1&&touchedLen<touched.length)touched[touchedLen++]=sid;"
    <> "if(c>best){best=c;bestSid=sid;"
    <> "}"
    <> "}"
    <> "for(let k=0;k<touchedLen;k++)counts[touched[k]]=0;"
    <> "return bestSid;"
    <> "}"

lifeLutFinishStepJs :: Text
lifeLutFinishStepJs =
  "var _speciesCounts=new Uint16Array("
    <> T.pack (show speciesCount)
    <> ");"
    <> "var _speciesTouched=new Uint16Array(256);"
    <> "function finishStep("
    <> "alive,species,nextAlive,nextSpecies,gridA,gridB,lut,w,h,x0,y0,x1,y1,"
    <> "nextLiveList,nextChangedList,stepCtx){"
    <> "w=w|0;h=h|0;"
    <> "if(w<=0||h<=0||!alive||!species||!nextAlive||!nextSpecies||!gridA||!gridB||!lut)"
    <> "return false;"
    <> "const n=(w*h)|0;"
    <> "const counts=_speciesCounts,touched=_speciesTouched;"
    <> "const xStart=Math.max(0,Math.floor(x0)-1);"
    <> "const yStart=Math.max(0,Math.floor(y0)-1);"
    <> "const xStop=Math.min(w,Math.floor(x1)+2);"
    <> "const yStop=Math.min(h,Math.floor(y1)+2);"
    <> "const copyFull=(yStop-yStart)*(xStop-xStart)*2>=n;"
    <> "if(copyFull){"
    <> "for(let i=0;i<n;i++)gridA[i]=alive[i]&1;"
    <> "stepRegionLUT(lut,gridA,gridB,w,h,0,h);"
    <> "}else{"
    <> "const copyY0=Math.max(0,yStart-1);"
    <> "const copyYStop=Math.min(h,yStop+1);"
    <> "for(let y=copyY0;y<copyYStop;y++){"
    <> "const row=y*w;"
    <> "for(let x=0;x<w;x++)gridA[row+x]=alive[row+x]&1;"
    <> "}"
    <> "stepRegionLUT(lut,gridA,gridB,w,h,yStart,yStop);"
    <> "}"
    <> "const grid=gridB;"
    <> "let pop=0,bx0=1e9,by0=1e9,bx1=-1,by1=-1,liveLen=0,changedLen=0;"
    <> "for(let y=yStart;y<yStop;y++){"
    <> "const row=y*w;"
    <> "for(let x=xStart;x<xStop;x++){"
    <> "const i=row+x;"
    <> "const was=alive[i]&1;"
    <> "const now=grid[i]&1;"
    <> "if(now&&was){"
    <> "nextAlive[i]=grid[i];nextSpecies[i]=species[i];"
    <> "}else if(!now){"
    <> "nextAlive[i]=0;nextSpecies[i]=0;"
    <> "}else{"
    <> "nextAlive[i]=grid[i];"
    <> "nextSpecies[i]=pickBirthSpecies(alive,species,w,h,x,y,counts,touched);"
    <> "}"
    <> "if(now){"
    <> "pop++;"
    <> "if(x<bx0)bx0=x;if(y<by0)by0=y;"
    <> "if(x>bx1)bx1=x;if(y>by1)by1=y;"
    <> "nextLiveList[liveLen++]=i;"
    <> "if(was!==now)nextChangedList[changedLen++]=i;"
    <> "}else if(was){"
    <> "nextChangedList[changedLen++]=i;"
    <> "}"
    <> "}"
    <> "}"
    <> "nextLiveList.length=liveLen;"
    <> "nextChangedList.length=changedLen;"
    <> "if(pop>0&&bx1>=bx0&&by1>=by0){"
    <> "refreshPackedRegion(nextAlive,w,h,bx0,by0,bx1,by1);"
    <> "}else if(x1>=x0&&y1>=y0){"
    <> "refreshPackedRegion(nextAlive,w,h,x0,y0,x1,y1);"
    <> "}"
    <> "stepCtx.pop=pop;"
    <> "stepCtx.bx0=bx0;"
    <> "stepCtx.by0=by0;"
    <> "stepCtx.bx1=bx1;"
    <> "stepCtx.by1=by1;"
    <> "return true;"
    <> "}"

lifeLutRuntimeFnsJs :: Text
lifeLutRuntimeFnsJs =
  lifeLutStepCoreJs
    <> lifeLutRefreshPackedJs
    <> lifeLutPickBirthJs
    <> lifeLutFinishStepJs

lifeLutEnsureJs :: Text
lifeLutEnsureJs =
  "var _g=typeof globalThis!=='undefined'?globalThis:self;"
    <> "if(!_g.__jsharkLifeLut||typeof _g.__jsharkLifeLut.finishStep!=='function'){"
    <> lifeLutRuntimeFnsJs
    <> "_g.__jsharkLifeLut={createLifeLUT,stepRegionLUT,finishStep,refreshPackedRegion};"
    <> "}"

-- IIFE must not end with (); JShark 'ffi' appends () at codegen.
lifeLutInstallJs :: Text
lifeLutInstallJs =
  "(function(){" <> lifeLutEnsureJs <> "})"

lifeLutWorkerBootJs :: Text
lifeLutWorkerBootJs =
  "(function(global){"
    <> lifeLutStepCoreJs
    <> "global.LifeLUT={createLifeLUT,stepRegionLUT};"
    <> "})(typeof self!=='undefined'?self:globalThis);"
