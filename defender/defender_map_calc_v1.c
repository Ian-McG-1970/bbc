#include <windows.h>   	// required for all Windows applications
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define LT <
#define LE <=
#define GT >
#define GE >=
#define NE !=
#define EQ ==

FILE *file;
char string[65535];

unsigned char map[1024];

const void debugstring() { file=fopen("log.txt","a"); fprintf(file,string); fclose(file); }

typedef struct Block
{
	unsigned char bitmap[4];
} Block;

unsigned char MapHeightPos0[256]; // the height at each of the 256 map pos
unsigned char MapHeightPos1[256];
unsigned char MapHeightPos2[256];
unsigned char MapHeightPos3[256];

unsigned char MapBlockPos0[256]; // the block to draw at each of the 256 map pos
unsigned char MapBlockPos1[256];
unsigned char MapBlockPos2[256];
unsigned char MapBlockPos3[256];

Block Blocks[256];
unsigned char BlackCount;

const unsigned char min_height(const unsigned char first, const unsigned char second)
{
	if (first LT second) return first;
	return second;
}

const unsigned char lowest(const unsigned char *height)
{
	unsigned char lowest=min_height(height[0], height[1]);
	lowest=min_height(lowest, height[2]);
	lowest=min_height(lowest, height[3]);
//	sprintf(string,"l %i h %i %i %i %i\n",lowest, height[0], height[1], height[2], height[3]); debugstring();
	return lowest;
}

const unsigned char calc_map_block(const unsigned char *bitmap)
{
	for (int b=0; b!=BlackCount; ++b)
	{
		if ( (Blocks[b].bitmap[0]==bitmap[0]) && (Blocks[b].bitmap[1]==bitmap[1]) && (Blocks[b].bitmap[2]==bitmap[2]) && (Blocks[b].bitmap[3]==bitmap[3]) )
		{
			return b;
		}
	}
	Blocks[BlackCount].bitmap[0]=bitmap[0];
	Blocks[BlackCount].bitmap[1]=bitmap[1];
	Blocks[BlackCount].bitmap[2]=bitmap[2];
	Blocks[BlackCount].bitmap[3]=bitmap[3];
	++BlackCount;
	return BlackCount-1;
} 

const Block build_block(const unsigned char lowest_height, const unsigned char *height)
{
	unsigned char pos[4];
	pos[0] = height[0] - lowest_height;
	pos[1] = height[1] - lowest_height;
	pos[2] = height[2] - lowest_height;
	pos[3] = height[3] - lowest_height;

//	sprintf(string,"lh %2i h %2i %2i %2i %2i p %2i %2i %2i %2i\n",lowest_height, height[0], height[1], height[2], height[3], pos[0], pos[1], pos[2], pos[3]); debugstring();

	unsigned char bitmap_result[4]={0,0,0,0};
	const unsigned char bitmap_value[4]={0x88, 0x44, 0x22, 0x11};
//	bitmap_result[0]=bitmap_result[1]=bitmap_result[2]=bitmap_result[3]=0;

	bitmap_result[pos[0]]=bitmap_value[0] + bitmap_result[pos[0]];
	bitmap_result[pos[1]]=bitmap_value[1] + bitmap_result[pos[1]];
	bitmap_result[pos[2]]=bitmap_value[2] + bitmap_result[pos[2]];
	bitmap_result[pos[3]]=bitmap_value[3] + bitmap_result[pos[3]];

//	sprintf(string,"lh %2i h %2i %2i %2i %2i p %2i %2i %2i %2i br %2x %2x %2x %2x\n",lowest_height, height[0],height[1],height[2],height[3], pos[0],pos[1],pos[2],pos[3], bitmap_result[0],bitmap_result[1],bitmap_result[2],bitmap_result[3]); debugstring();

	Block blk;
	blk.bitmap[0] = bitmap_result[0];
	blk.bitmap[1] = bitmap_result[1];
	blk.bitmap[2] = bitmap_result[2];
	blk.bitmap[3] = bitmap_result[3];

//	sprintf(string,"lh %i h %i %i %i %i p %i %i %i %i\n",lowest_height, height[0], height[1], height[2], height[3], pos[0], pos[1], pos[2], pos[3]); debugstring();
//	sprintf(string,"blk, %i, %i, %i, %i\n",blk.bitmap[0], blk.bitmap[1], blk.bitmap[2], blk.bitmap[3]); debugstring();

	return blk;
}

const void build_blocks_from_map(unsigned char *height_map, unsigned char *block_map)
{
	sprintf(string,"build_blocks_from_map\n"); debugstring();
	printf("build_blocks_from_map\n");

	for (int b=0, c=0; b!=256; ++b, c+=4)
	{
		height_map[b]	= lowest(&map[c]);
		Block blk		= build_block(height_map[b], &map[c]);
		block_map[b]	= calc_map_block(&blk.bitmap[0]);
	sprintf(string,"bbfm seg %3i b %3i hgt %2i blk %2i\n",b*4/32,b,height_map[b],block_map[b]); debugstring();
//	printf("build_blocks_from_map\n");
	}
}

const void rotate_map()
{
	sprintf(string,"rotate_map\n"); debugstring();
	const unsigned char temp=map[0];
	for (int p=0; p!=1023; ++p)
	{
		map[p]=map[p+1];
	}
	map[1023]=temp;
}

// 256 blocks with height between 0 and 15

// set start height
// expand 32 bytes into 256 bits
// for each of the 256 bits
//  work out if this bit is up(0) or down(1)
//   set pos*4 as curr height +4(down=1) or -4(up=0)
//   fill in nxxt 3 positions based on whether its up or down
//   recalc curr height based on whether its up or down

const void bits_to_bytes(unsigned char bits, unsigned char *bytes)
{
	sprintf(string,"bits_to_bytes\n"); debugstring();
	for (int bitpos=0; bitpos!=8; ++bitpos)
	{
		bytes[bitpos]=bits &1;
		bits = bits >>1;
	}
}

const void bitmap_to_bytes(unsigned char *bits, unsigned char *bytes, const unsigned int length)
{
	sprintf(string,"bitmap_to_bytes\n"); debugstring();

	for (int bit=0, byte=0; byte!=length; ++bit, byte+=8)
	{
		bits_to_bytes(bits[bit], &bytes[byte]);
	}	
}

const void output(unsigned char *map)
{
	sprintf(string,"output\n"); debugstring();
	printf("output\n");

	sprintf(string,"\n"); debugstring();
	for (int c=0; c!=1024; ++c)
	{
		sprintf(string," %2i", map[c]); debugstring();
	}
	sprintf(string,"\n"); debugstring();
}

unsigned char plot[1024][64];

const void PlotLandscape(const unsigned char *map)
{
	sprintf(string,"PlotLandscape\n"); debugstring();
	unsigned char plot[1024][64];

	for (int v=0; v!=64; ++v)
	{
		for (int h=0; h!=1024; ++h)
		{
			plot[h][v]=' ';
		}
	}
	for (int h=0; h!=1024; ++h)
	{
		const unsigned char v=map[h];
		plot[h][v]='X';
	}
	for (int v=0; v!=64; ++v)
	{
		sprintf(string,"%2i ",v); debugstring();
		for (int h=0; h!=1024; ++h)
		{
			sprintf(string,"%c",plot[h][v]); debugstring();
		}
		sprintf(string,"\n"); debugstring();
	}
}


const unsigned char txt[1025]={"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"};

const unsigned char m00[1025]={"                                                                                               x                                                                                                                                                                 x                                                                                                                                                                                                                                                                                                                     x                                                     x                                                                                                                                                                                                                                                                                             x                                                                                                                    "};
const unsigned char m01[1025]={"                                                                                              x x                                                                                                                                                               x x           x                                                                                                                                                                                                                                                                                                       x x                                                   x x                                                                                                                                                                                                                                                                                           x x                                                                                                                   "};               
const unsigned char m02[1025]={"                                                                                             x   x                                                                                                                                                             x   x   x     x x                                                                                                                                                                                                                                                                                                     x   x                                                 x   x                                                                                                                                                                                                                                                                                         x   x                                                                                                                  "};                       
const unsigned char m03[1025]={"                                                                                            x     x                                                                                                                                                           x     x x x   x   x                                                                                                                                                                                                                                                                                                   x     x                                               x     x                                                                                                                                                                                                                                                                                       x     x                                                                                                                 "};                    
const unsigned char m04[1025]={"                                                                                           x       x                                                                                                                                                         x       x   x x     x                                                                                                                                                                                                                                                                                                 x       x                                             x       x                                                                                                                                                                                                                                                                                     x       x                                                                                                                "};                    
const unsigned char m05[1025]={"                                                                                          x         x                                                                                                                                                       x             x       x                                                                                                                                                                                                                                                                                               x         x                                           x         x                                                                                                                                                                                                                                                                                   x         x                                                                                                               "};                    
const unsigned char m06[1025]={"                                                                                         x           x                                                                                                                                 x                   x                       x                                                                                                                                                                                                                                                                                       x     x           x                                         x           x                                                                                                                                                                                                                                                                                 x           x                                                                                                              "};          
const unsigned char m07[1025]={"                                                                                        x             x                                                                                                                               x x                 x                         x                                                                                                                                                                                                                                                                                     x x   x             x                         x             x             x                                                                                                                                                                                                                                                                               x             x                                                                                                             "};            
const unsigned char m08[1025]={"                                                                                       x               x                                                                                                                             x   x               x                           x                                                                                                                                                                                                                                                                                   x   x x               x                       x x           x               x                                                                                                                                                                                                                                                                             x               x                                                                                                            "};            
const unsigned char m09[1025]={"                                                                                      x                 x                                                                                                                           x     x             x                             x                                                                                                                                                                                                                                                                                 x     x                 x                     x   x         x                 x                                                                                                                                                                                                                                                                           x                 x                                                                                                           "};          
const unsigned char m10[1025]={"                                                                                     x                   x                                                                                                                         x       x           x                               x     x                                                                                                                                                                                                                                                                   x     x                         x                   x     x       x                   x                                                                                                                                                                                                                                                                         x                   x                                                                                                          "};          
const unsigned char m11[1025]={"                                                                                    x                     x                                                                                                                       x         x         x                                 x   x x                                                                                                                                                                                                                                                                 x x   x                           x                 x       x     x                     x                                                                                                                                                                                                                                                                       x                     x                                                                                                         "};           
const unsigned char m12[1025]={"                                                                                   x                       x                                                                                                                     x           x       x                                   x x   x                                                                                                                                                                                                                                                               x   x x                             x               x         x   x                       x                                                                                                                                                                                                                                                             x       x                       x                                                                                                        "};           
const unsigned char m13[1025]={"                                                                                  x                         x                                                                                       x                           x             x     x                                     x     x                                                                                                                                                                                                                                                             x     x                               x             x           x x                         x                                                                                                                                                                                                                                                           x x     x                         x                                                                                                       "};          
const unsigned char m14[1025]={"                                                                                 x                           x                                                                                     x x                         x               x   x                                             x                                                                                                                                                                                                                                                           x                                       x           x             x                           x                                                                                                                                                                                                                                                         x   x   x                           x                                                                                                      "};       
const unsigned char m15[1025]={"                                                                                x                             x                                                                                   x   x                       x                 x x                                               x                                                                                                                                                                                                                                                         x                                         x         x                                           x     x                                                                                                                                                                                                                                                 x     x x                             x                                                                                                     "};      
const unsigned char m16[1025]={"                                                                               x                               x                                                                                 x     x                     x                   x                                                 x     x                                                                                                                                                                                                                                             x   x                                           x       x                                             x   x x                                                                                                                                                                                                                                               x       x                               x                                                                                                    "};      
const unsigned char m17[1025]={"                                                                              x                                 x                                                                               x       x                   x                                                                       x   x x                                                                                                                                                                                                                                           x x x                                             x     x                                               x x   x                                                                                                                                                                                                                                             x                                         x                                                                                                   "};     
const unsigned char m18[1025]={"                                                                             x                                   x                                                                             x         x                 x                                                                         x x   x                                                                                                                                                                                                                                         x   x                                               x   x                                                 x     x                                                                                                                                                                                                                                           x                                           x                                                                                                  "};  
const unsigned char m19[1025]={"                                                                            x                                     x                                                             x             x           x               x                                                                           x     x                                                                                                                                                                                                                                       x                                                     x x                                                         x                                                                                                                                                                                                                                         x                                             x                                                                                                 "}; 
const unsigned char m20[1025]={"                                                                           x                                       x                                                           x x           x             x             x                                                                                   x                                                                                                                                                                 x                                                                   x                                                       x                                                           x                                                                                                                                                                                                                                       x                                               x                                                                                                "}; 
const unsigned char m21[1025]={"                                                                          x                                         x                                                         x   x         x               x           x                                                                                     x                                                                                                                                                               x x                                                                 x                                                                                                                     x                                                                                                                                                                                                                                     x                                                 x                                                                                               "};                                                                    
const unsigned char m22[1025]={"                                                                         x                                           x                                                       x     x       x                 x         x                                                                                       x                                                                                                                                                             x   x                                                               x                                                                                                                       x                                                                                                                                                                                                                                   x                                                   x                                                                                              "};
const unsigned char m23[1025]={"                                                                        x                                             x                                                     x       x     x                   x       x                                                                                         x                                                                                                                                         x                 x     x                                                             x                                                                                                                         x                                                                                                                                                                                                                                 x                                                     x                         x                                                         x         "};                                    
const unsigned char m24[1025]={"                                                                       x                                               x                                                   x         x   x                     x     x                                                                                           x                                                                                                                                       x x               x       x                                                           x                                                                                                                           x                                                                                                                                                                                                                               x                                                       x                       x x                                                       x x        "};                                       
const unsigned char m25[1025]={"                                                                      x                                                 x                                                 x           x x                       x   x                                                                                             x                                                                                                                                     x   x             x         x                                                         x                                                                                                                             x                                                                                                                                                                                                                             x                                                         x                     x   x                                                     x   x       "};                                      
const unsigned char m26[1025]={"                                                                     x                                                   x                                               x             x                         x x                                                                                               x                                                                                                                                   x     x           x           x                                           x           x                                                                                                                               x                                                                                                                                                                                                                           x                                                           x                   x     x                                                   x     x      "};                                     
const unsigned char m27[1025]={"                                                                    x                                                     x                                             x                                         x                                                                                                 x           x                                                                                                                     x       x         x             x                   x                     x x         x                                                                                                                                 x                                                                                                                                                                                                                         x                                                             x                 x       x                                                 x       x     "};                                        
const unsigned char m28[1025]={"                                                                   x                                                       x           x                               x                                                                                                                                             x         x x                                                                                                                   x         x       x               x x x x           x x                   x   x       x                                                                                                                                   x                                                                                                                                                                                                                       x                                                               x               x         x                                               x         x    "};                                           
const unsigned char m29[1025]={"                                                                  x                                                         x         x x                             x                                                                                                                                               x       x   x                                                                                                                 x           x     x                 x x x x         x   x                 x     x     x                                                                                                                                     x                                                                                                                                                                                                                     x                                                                 x             x           x                                             x           x   "};                            
const unsigned char m30[1025]={"                                                                 x                                                           x       x   x                           x                                                                                                                                                 x     x     x                                                                                                               x             x   x                         x       x     x               x       x   x                                                                                                                                       x                                                                                                                                                                                                                   x                                                                   x           x             x                                           x             x  "};
const unsigned char m31[1025]={"x                                                               x                                                             x     x     x                         x                                                                                                                                                   x   x       x                                                                                                             x               x x                           x     x       x             x         x x                                                                                                                                         x       x                                                                                                                                                                                                         x                                                                     x         x               x                                         x               x "};
const unsigned char m32[1025]={" x                                                             x                                                               x   x       x                       x                                                                                                                                                     x x         x                                                                                                           x                 x                             x   x         x           x           x                                                                                                                                           x     x x                                                                                                                                                                                                       x                                                                       x       x                 x                                       x                 x"};                                                                   
const unsigned char m33[1025]={"  x                                                           x                                                                 x x         x                     x                                                                                                                                                       x           x                                                                                                   x     x                                                 x x           x         x                                                                                                                                                         x   x   x                                                                                                                                                                                                     x                                                                         x     x                   x                                     x                   "};                                                
const unsigned char m34[1025]={"   x                                                         x                                                                   x           x                   x                                                                                                                                                                     x                                                                                                 x x   x                                                   x             x       x                                                                                                                                                           x x     x                                                                                                                                                                                                   x                                                                           x   x                     x                                   x                    "};                                             
const unsigned char m35[1025]={"    x                                                       x                                                                                 x                 x                                                                                                                                                                       x                                                                                               x   x x                                                                   x     x                                                                                                                                                             x       x                                                                                                                                                                                                 x                                                                             x x                       x                                 x                     "};                                         
const unsigned char m36[1025]={"     x                                                     x                                                                                   x               x                                                                                                                                                                         x                                                                                             x     x                                                                     x   x                                                                                                                                                                       x                                                                                                                                                                                               x                                                                               x                         x                               x                      "};                                      
const unsigned char m37[1025]={"      x                                                   x                                                                                     x             x                                                                                                                                                                           x                                                                                           x                                                                             x x                                                                                                                                                                         x                                                                                                                                                                                             x                                                                                                           x                             x                       "};                                 
const unsigned char m38[1025]={"       x                                                 x                                                                                       x           x                                                                                                                                                                             x                                                                                         x                                                                               x                                                                                                                                                                           x                                                                                                                                                                                           x                                                                                                             x                           x                        "};                                
const unsigned char m39[1025]={"        x                                               x                                                                                         x         x                                                                                                                                                                               x                                                                                       x                                                                                                                                                                                                                                                             x                                                                                                                                                                                         x                                                                                                               x                         x                         "};                              
const unsigned char m40[1025]={"         x                                             x                                                                                           x       x                                                                                                                                                                                 x                                                                                     x                                                                                                                                                                                                                                                               x   x     x                                                                                                                                                                             x                                                                                                                 x                       x                          "};                               
const unsigned char m41[1025]={"          x                                           x                                                                                             x     x                                                                                                                                                                                   x                                                                                 x x                                                                                                                                                                                                                                                                 x x x   x x     x x x x x                                                                                                                                                             x                                                                                                                   x                     x                           "};                                      
const unsigned char m42[1025]={"           x                                         x                                                                                               x   x                                                                                                                                                                                     x                                                                               x x                                                                                                                                                                                                                                                                   x   x x   x   x x x x x x                                                                                                                                                           x                                                                                                                     x     x             x                            "};                                      
const unsigned char m43[1025]={"            x                                       x                                                                                                 x x                                                                                                                                                                                       x                                                                             x                                                                                                                                                                                                                                                                           x     x x           x                                                           x                                                                                         x   x                                                                                                                       x   x x           x                             "};                                 
const unsigned char m44[1025]={"             x                                     x                                                                                                   x                                                                                                                                                                                         x                                                                           x                                                                                                                                                                                                                                                                                   x             x         x                                               x x                                                                                       x x x                                                                                                                         x x   x         x                              "};                                
const unsigned char m45[1025]={"              x                                   x                                                                                                                                                                                                                                                                                               x                                                                     x   x                                                                                                                                                                                                                                                                                                   x       x x                                             x   x                                                                                     x   x                                                                                                                           x     x       x                               "};                             
const unsigned char m46[1025]={"               x                                 x                                                                                                                                                                                                                                                                                                 x     x                                                             x x x                                                                                                                                                                                                                                                                                                     x     x   x                                           x     x     x                                                                             x                                                                                                                                       x     x                                "};                            
const unsigned char m47[1025]={"                x                               x                                                                                                                                                                                                                                                                                                   x   x x                                                           x   x                                                                                                                                                                                                                                                                                                       x   x     x                                         x       x   x x                                 x                                         x                                                                                                                                         x   x                                 "};                              
const unsigned char m48[1025]={"                 x                             x                                                                                                                                                                                                                                                                                                     x x   x                                                         x                                                                                                                                                                                                                                                                                                             x x       x                                       x         x x   x                               x x                                       x                                                                                                                                           x x                                  "};                            
const unsigned char m49[1025]={"                  x                           x                                                                                                                                                                                                                                                                                                       x     x                                                   x   x                                                                                                                                                                                                                                                                                                               x         x                                     x           x     x                             x   x                                     x                                                                                                                                             x                                   "};                         
const unsigned char m50[1025]={"                   x                         x                                                                                                                                                                                                                                                                                                               x                                                 x x x                                                                                                                                                                                                                                                                                                                           x                                   x                   x         x                 x     x                               x   x                                                                                                                                                                                  "};
const unsigned char m51[1025]={"                    x                       x                                                                                                                                                                                                                                                                                                                 x                                               x   x                                                                                                                                                                                                                                                                                                                             x                                 x                     x       x x               x       x                             x x x                                                                                                                                                                                   "};
const unsigned char m52[1025]={"                     x                     x                                                                                                                                                                                                                                                                                                                   x     x                                       x                                                                                                                                                                                                                                                                                                                                   x                               x                       x     x   x             x         x                           x   x                                                                                                                                                                                    "};  
const unsigned char m53[1025]={"                      x                   x                                                                                                                                                                                                                                                                                                                     x   x x                       x             x                                                                                                                                                                                                                                                                                                                                     x                   x         x                         x   x     x           x           x                   x     x                                                                                                                                                                                         "};
const unsigned char m54[1025]={"                       x                 x                                                                                                                                                                                                                                                                                                                       x x   x                     x x           x                                                                                                                                                                                                                                                                                                                                       x                 x x       x                           x x       x         x             x                 x x   x                                                                                                                                                                                          "};
const unsigned char m55[1025]={"                        x               x                                                                                                                                                                                                                                                                                                                         x     x                   x   x         x                                                                                                                                                                                                                                                                                                                                         x               x   x     x                             x         x       x               x               x   x x                                                                                                                                                                                           "};
const unsigned char m56[1025]={"                         x             x                                                                                                                                                                                                                                                                                                                                 x                 x     x       x                                                                                                                                                                                                                                                                                                                                           x             x     x   x                                         x     x                 x             x     x                                                                                                                                                                                            "};
const unsigned char m57[1025]={"                          x           x                                                                                                                                                                                                                                                                                                                                   x               x       x     x                                                                                                                                                                                                                                                                                                                                             x           x       x x                                           x   x                   x           x                                                                                                                                                                                                   "};
const unsigned char m58[1025]={"                           x         x                                                                                                                                                                                                                                                                                                                                     x           x x         x   x                                                                                                                                                                                                                                                                                                                                               x         x         x                                             x x                     x         x                                                                                                                                                                                                    "};
const unsigned char m59[1025]={"                            x       x                                                                                                                                                                                                                                                                                                                                       x x       x x           x x                                                                                                                                                                                                                                                                                                                                                 x       x                                                         x                       x       x                                                                                                                                                                                                     "};
const unsigned char m60[1025]={"                             x     x                                                                                                                                                                                                                                                                                                                                         x x     x               x                                                                                                                                                                                                                                                                                                                                                   x     x                                                                                   x     x                                                                                                                                                                                                      "};
const unsigned char m61[1025]={"                              x   x                                                                                                                                                                                                                                                                                                                                             x   x                                                                                                                                                                                                                                                                                                                                                                     x   x                                                                                     x   x                                                                                                                                                                                                       "};
const unsigned char m62[1025]={"                               x x                                                                                                                                                                                                                                                                                                                                               x x                                                                                                                                                                                                                                                                                                                                                                       x x                                                                                       x x                                                                                                                                                                                                        "};
const unsigned char m63[1025]={"                                x                                                                                                                                                                                                                                                                                                                                                 x                                                                                                                                                                                                                                                                                                                                                                         x                                                                                         x                                                                                                                                                                                                         "};                                                                                                                                                                                                  

const unsigned char Height(const int column)
{
	if (m00[column] NE ' ') return 0;
	if (m01[column] NE ' ') return 1;
	if (m02[column] NE ' ') return 2;
	if (m03[column] NE ' ') return 3;
	if (m04[column] NE ' ') return 4;
	if (m05[column] NE ' ') return 5;
	if (m06[column] NE ' ') return 6;
	if (m07[column] NE ' ') return 7;
	if (m08[column] NE ' ') return 8;
	if (m09[column] NE ' ') return 9;

	if (m10[column] NE ' ') return 10;
	if (m11[column] NE ' ') return 11;
	if (m12[column] NE ' ') return 12;
	if (m13[column] NE ' ') return 13;
	if (m14[column] NE ' ') return 14;
	if (m15[column] NE ' ') return 15;
	if (m16[column] NE ' ') return 16;
	if (m17[column] NE ' ') return 17;
	if (m18[column] NE ' ') return 18;
	if (m19[column] NE ' ') return 19;

	if (m20[column] NE ' ') return 20;
	if (m21[column] NE ' ') return 21;
	if (m22[column] NE ' ') return 22;
	if (m23[column] NE ' ') return 23;
	if (m24[column] NE ' ') return 24;
	if (m25[column] NE ' ') return 25;
	if (m26[column] NE ' ') return 26;
	if (m27[column] NE ' ') return 27;
	if (m28[column] NE ' ') return 28;
	if (m29[column] NE ' ') return 29;

	if (m30[column] NE ' ') return 30;
	if (m31[column] NE ' ') return 31;
	if (m32[column] NE ' ') return 32;
	if (m33[column] NE ' ') return 33;
	if (m34[column] NE ' ') return 34;
	if (m35[column] NE ' ') return 35;
	if (m36[column] NE ' ') return 36;
	if (m37[column] NE ' ') return 37;
	if (m38[column] NE ' ') return 38;
	if (m39[column] NE ' ') return 39;

	if (m40[column] NE ' ') return 40;
	if (m41[column] NE ' ') return 41;
	if (m42[column] NE ' ') return 42;
	if (m43[column] NE ' ') return 43;
	if (m44[column] NE ' ') return 44;
	if (m45[column] NE ' ') return 45;
	if (m46[column] NE ' ') return 46;
	if (m47[column] NE ' ') return 47;
	if (m48[column] NE ' ') return 48;
	if (m49[column] NE ' ') return 49;

	if (m50[column] NE ' ') return 50;
	if (m51[column] NE ' ') return 51;
	if (m52[column] NE ' ') return 52;
	if (m53[column] NE ' ') return 53;
	if (m54[column] NE ' ') return 54;
	if (m55[column] NE ' ') return 55;
	if (m56[column] NE ' ') return 56;
	if (m57[column] NE ' ') return 57;
	if (m58[column] NE ' ') return 58;
	if (m59[column] NE ' ') return 59;

	if (m60[column] NE ' ') return 60;
	if (m61[column] NE ' ') return 61;
	if (m62[column] NE ' ') return 62;
//	if (m63[column] NE ' ')
 return 63;
}

// todo - check for gaps greater than 1

const void Gaps()
{
	sprintf(string,"Gaps\n"); debugstring();		
	printf("Gaps\n");

	for (int c=0; c!=1024; ++c)
	{
		const int p=(c-1) &1023;

	sprintf(string,"Gaps %4i %4i %2i %2i\n",p,c,map[p],map[c]); debugstring();		
	printf("Gaps %4i %4i %2i %2i\n",p,c,map[p],map[c]);			


		if ( abs ( map[p]-map[c] ) NE 1)


//if ( (abs (map[p]-map[c]) ) <> 1)
		{
	sprintf(string,"Gaps error %4i %4i %2i %2i\n",p,c,map[p],map[c]); debugstring();		
	printf("Gaps error %4i %4i %2i %2i\n",p,c,map[p],map[c]);			
		}		
	}
}

const void BuildMap()
{
	sprintf(string,"BuildMap1024\n"); debugstring();		
	printf("BuildMap1024\n");

	for (int c=0; c!=1024; ++c)
	{
		map[c]=Height(c);
	sprintf(string,"BuildMap1024 %3i %2i\n",c,map[c]); debugstring();		
	printf("BuildMap1024 %3i %2i\n",c,map[c]);

	}
}

const void MapPositionHeightCount(unsigned char pos, unsigned char *height, unsigned char *count)
{
	*height=255;
	*count=0;
	if (m00[pos] NE ' ') { ++*count; *height=0; };
	if (m01[pos] NE ' ') { ++*count; *height=1; };
	if (m02[pos] NE ' ') { ++*count; *height=2; };
	if (m03[pos] NE ' ') { ++*count; *height=3; };
	if (m04[pos] NE ' ') { ++*count; *height=4; };
	if (m05[pos] NE ' ') { ++*count; *height=5; };
	if (m06[pos] NE ' ') { ++*count; *height=6; };
	if (m07[pos] NE ' ') { ++*count; *height=7; };
	if (m08[pos] NE ' ') { ++*count; *height=8; };
	if (m09[pos] NE ' ') { ++*count; *height=9; };

	if (m04[pos] NE ' ') { ++*count; *height=4; };
	if (m08[pos] NE ' ') { ++*count; *height=8; };
	if (m12[pos] NE ' ') { ++*count; *height=12; };
	if (m16[pos] NE ' ') { ++*count; *height=16; };
	if (m20[pos] NE ' ') { ++*count; *height=20; };
	if (m24[pos] NE ' ') { ++*count; *height=24; };
	if (m28[pos] NE ' ') { ++*count; *height=28; };
	if (m32[pos] NE ' ') { ++*count; *height=32; };
	if (m36[pos] NE ' ') { ++*count; *height=36; };
	if (m40[pos] NE ' ') { ++*count; *height=40; };
	if (m44[pos] NE ' ') { ++*count; *height=44; };
	if (m48[pos] NE ' ') { ++*count; *height=48; };
	if (m52[pos] NE ' ') { ++*count; *height=52; };
	if (m56[pos] NE ' ') { ++*count; *height=56; };
	if (m60[pos] NE ' ') { ++*count; *height=60; };
}


//fc fa cc a0 96 94 92 90 8e 8c 8a 88 86 84 82 80 old
//7a 78 76 74 72 70 6e 6c 6a 68 66 64 62 60 5e 5c new

#define line00 0x020
#define line01 0x022
#define line02 0x024
#define line03 0x026
#define line04 0x028
#define line05 0x02a
#define line06 0x02c
#define line07 0x02e
#define line08 0x030
#define line09 0x032
#define line10 0x034
#define line11 0x036
#define line12 0x038
#define line13 0x03a
#define line14 0x03c
#define line15 0x03e
#define line16 0x040
#define line17 0x042
#define line18 0x044
#define line19 0x046
#define line20 0x048
#define line21 0x04a
#define line22 0x04c
#define line23 0x04e
#define line24 0x050
#define line25 0x052
#define line26 0x054
#define line27 0x056
#define line28 0x058
#define line29 0x05a
#define line30 0x05c
#define line31 0x05e
#define line32 0x060
#define line33 0x062
#define line34 0x064
#define line35 0x066
#define line36 0x068
#define line37 0x06a
#define line38 0x06c
#define line39 0x06e
#define line40 0x070
#define line41 0x072
#define line42 0x074
#define line43 0x076
#define line44 0x078
#define line45 0x07a
#define line46 0x07c
#define line47 0x07e
#define line48 0x080
#define line49 0x082
#define line50 0x084
#define line51 0x086
#define line52 0x088
#define line53 0x08a
#define line54 0x08c
#define line55 0x08e
#define line56 0x090
#define line57 0x092
#define line58 0x094
#define line59 0x096
#define line60 0x098
#define line61 0x09a
#define line62 0x09c
#define line63 0x09e

/*
#define line00 0x020
#define line01 0x024
#define line02 line01
#define line03 line02
#define line04 line03
#define line05 line04
#define line06 line05
#define line07 line06
#define line08 line07
#define line09 line08
#define line10 line09
#define line11 line10
#define line12 line11
#define line13 line12
#define line14 line13
#define line15 line14
#define line16 line15
#define line17 line16
#define line18 line17
#define line19 line18
#define line20 line19
#define line21 line20
#define line22 line21
#define line23 line22
#define line24 line23
#define line25 line24
#define line26 line25
#define line27 line26
#define line28 line27
#define line29 line28
#define line30 line29
#define line31 line30
#define line32 line31
#define line33 line32
#define line34 line33
#define line35 line34
#define line36 line35
#define line37 line36
#define line38 line37
#define line39 line38
#define line40 line39
#define line41 line40
#define line42 line41
#define line43 line42
#define line44 line43
#define line45 line44
#define line46 line45
#define line47 line46
#define line48 line47
#define line49 line48
#define line50 line49
#define line51 line50
#define line52 line51
#define line53 line52
#define line54 line53
#define line55 line54
#define line56 line55
#define line57 line56
#define line58 line57
#define line59 line58
#define line60 line59
#define line61 line60
#define line62 line61
#define line63 line62
*/
/*
#define line00 0x05c //80
#define line01 0x05e //82
#define line02 0x060 //84
#define line03 0x062 //86
#define line04 0x064 //88
#define line05 0x066 //8a
#define line06 0x068 //8c
#define line07 0x06a //8e
#define line08 0x06c //90
#define line09 0x06e //92
#define line10 0x070 //94
#define line11 0x072 //96
#define line12 0x098
#define line13 0x09a
#define line14 0x09c
#define line15 0x09e
#define line16 0x074 //a0
#define line17 0x0a2
#define line18 0x0a4
#define line19 0x0a6
#define line20 0x0a8
#define line21 0x0aa
#define line22 0x0ac
#define line23 0x0ae
#define line24 0x0b0
#define line25 0x0b2
#define line26 0x0b4
#define line27 0x0b6
#define line28 0x0b8
#define line29 0x0ba
#define line30 0x0bc
#define line31 0x0be
#define line32 0x0c0
#define line33 0x0c2
#define line34 0x0c4
#define line35 0x0c6
#define line36 0x0c8
#define line37 0x0ca
#define line38 0x076 //cc
#define line39 0x0ce
#define line40 0x0d0
#define line41 0x0d2
#define line42 0x0d4
#define line43 0x0d6
#define line44 0x0d8
#define line45 0x05a //da
#define line46 0x0dc
#define line47 0x0de
#define line48 0x0e0
#define line49 0x0e2
#define line50 0x0e4
#define line51 0x0e6
#define line52 0x0e8
#define line53 0x0ea
#define line54 0x0ec
#define line55 0x0ee
#define line56 0x0f0
#define line57 0x0f2
#define line58 0x058 //f4
#define line59 0x0f6
#define line60 0x0f8
#define line61 0x078 //fa
#define line62 0x07a //fc
#define line63 0x0fe
*/
const unsigned char heighttab[256]={
line00, line01, line02, line03,  
line01, line02, line03, line04, 
line02, line03, line04, line05, 
line03, line04, line05, line06, 
line04, line05, line06, line07, 
line05, line06, line07, line08, 
line06, line07, line08, line09, 
line07, line08, line09, line10, 
line08, line09, line10, line11, 
line09, line10, line11, line12, 
line10, line11, line12, line13, 
line11, line12, line13, line14, 
line12, line13, line14, line15, 
line13, line14, line15, line16, 
line14, line15, line16, line17, 
line15, line16, line17, line18, 
line16, line17, line18, line19, 
line17, line18, line19, line20, 
line18, line19, line20, line21, 
line19, line20, line21, line22, 
line20, line21, line22, line23, 
line21, line22, line23, line24, 
line22, line23, line24, line25, 
line23, line24, line25, line26, 
line24, line25, line26, line27, 
line25, line26, line27, line28, 
line26, line27, line28, line29, 
line27, line28, line29, line30, 
line28, line29, line30, line31, 
line29, line30, line31, line32, 
line30, line31, line32, line33, 
line31, line32, line33, line34, 
line32, line33, line34, line35, 
line33, line34, line35, line36, 
line34, line35, line36, line37, 
line35, line36, line37, line38, 
line36, line37, line38, line39, 
line37, line38, line39, line40, 
line38, line39, line40, line41, 
line39, line40, line41, line42, 
line40, line41, line42, line43, 
line41, line42, line43, line44, 
line42, line43, line44, line45,
line43, line44, line45, line46,
line44, line45, line46, line47,
line45, line46, line47, line48,
line46, line47, line48, line49,
line47, line48, line49, line50,
line48, line49, line50, line51,
line49, line50, line51, line52,
line50, line51, line52, line53,
line51, line52, line53, line54,
line52, line53, line54, line55,
line53, line54, line55, line56,
line54, line55, line56, line57,
line55, line56, line57, line58,
line56, line57, line58, line59,
line57, line58, line59, line60,
line58, line59, line60, line61,
line59, line60, line61, line62,
line60, line61, line62, line63,
line61, line62, line63, line63,
line62, line63, line63, line63,
line63, line63, line63, line63,
};

const unsigned char ZP[64]={
//128,130,132,134,136,138,140,142,144,146,
//148,150,152,154,156,158,160,162,164,166,
//168,170,172,174,176,178,180,182,184,186,
//188,190,192,194,196,198,200,202,204,206,
//208,210,212,214,216,218,220,222,224,226,
//228,230,232,234,236,238,240,242,244,246,
//248,250,252,254	
	line00,line01,line02,line03,line04,line05,line06,line07,line08,line09,
	line10,line11,line12,line13,line14,line15,line16,line17,line18,line19,
	line20,line21,line22,line23,line24,line25,line26,line27,line28,line29,
	line30,line31,line32,line33,line34,line35,line36,line37,line38,line39,
	line40,line41,line42,line43,line44,line45,line46,line47,line48,line49,
	line50,line51,line52,line53,line54,line55,line56,line57,line58,line59,
	line60,line61,line62,line63
	
//	0x080,0x082,0x084,0x086,0x088,0x08a,0x08c,0x08e,
//	0x090,0x092,0x094,0x096,0x098,0x09a,0x09c,0x09e,
//	0x0a0,0x0a2,0x0a4,0x0a6,0x0a8,0x0aa,0x0ac,0x0ae,
//	0x0b0,0x0b2,0x0b4,0x0b6,0x0b8,0x0ba,0x0bc,0x0be,
//	0x0c0,0x0c2,0x0c4,0x0c6,0x0c8,0x0ca,0x0cc,0x0ce,
//	0x0d0,0x0d2,0x0d4,0x0d6,0x0d8,0x0da,0x0dc,0x0de,
//	0x0e0,0x0e2,0x0e4,0x0e6,0x0e8,0x0ea,0x0ec,0x0ee,
//	0x0f0,0x0f2,0x0f4,0x0f6,0x0f8,0x0fa,0x0fc,0x0fe,

//254,252,250,248,246,244,242,240,238,236,
//234,232,230,228,226,224,222,220,218,216,
//214,212,210,208,206,204,202,200,198,196,
//194,192,190,188,186,184,182,180,178,176,
//174,172,170,168,166,164,162,160,158,156,
//154,152,150,148,146,144,142,140,138,136,
//134,132,130,128

//174,172,170,168,166,164,162,160,158,156,
//154,152,150,148,146,144,142,140,138,136,
//134,132,130,128,126,124,122,120,118,116,
//114,112,110,108,106,104,102,100, 98, 96,
// 94, 92, 90, 88, 86, 84, 82, 80, 78, 76,
// 74, 72, 70, 68, 66, 64, 62, 60, 58, 56,
// 54, 52, 50, 48
};

const void BuildBBCTable(const unsigned char *table, const int length)
{
	for (int p=0; p!=length; ++p) //height0
	{
		if ((p &15) ==0)
		{
			sprintf(string,"\n EQUB "); debugstring();
		}
		sprintf(string,"%3i, ",table[p]); debugstring();
	}	
}

const void BuildBBCTables()
{
	sprintf(string,"\n.MapHgtPos1"); debugstring();
	BuildBBCTable(&MapHeightPos0[0], 256);
	BuildBBCTable(&MapHeightPos0[0], 32);
	
	sprintf(string,"\n.MapHgtPos2"); debugstring();
	BuildBBCTable(&MapHeightPos1[0], 256);
	BuildBBCTable(&MapHeightPos1[0], 32);
	
	sprintf(string,"\n.MapHgtPos3"); debugstring();
	BuildBBCTable(&MapHeightPos2[0], 256);
	BuildBBCTable(&MapHeightPos2[0], 32);

	sprintf(string,"\n.MapHgtPos4"); debugstring();
	BuildBBCTable(&MapHeightPos3[0], 256);
	BuildBBCTable(&MapHeightPos3[0], 32);

	sprintf(string,"\n.MapBMTab1"); debugstring();
	BuildBBCTable(&MapBlockPos0[0], 256);
	BuildBBCTable(&MapBlockPos0[0], 32);

	sprintf(string,"\n.MapBMTab2"); debugstring();
	BuildBBCTable(&MapBlockPos1[0], 256);
	BuildBBCTable(&MapBlockPos1[0], 32);

	sprintf(string,"\n.MapBMTab3"); debugstring();
	BuildBBCTable(&MapBlockPos2[0], 256);
	BuildBBCTable(&MapBlockPos2[0], 32);

	sprintf(string,"\n.MapBMTab4"); debugstring();
	BuildBBCTable(&MapBlockPos3[0], 256);
	BuildBBCTable(&MapBlockPos3[0], 32);

	sprintf(string,"\n.BMTab1"); debugstring();
	for (int p=0; p!=BlackCount; ++p)
	{
		if ((p &15) ==0)
		{
			sprintf(string,"\n EQUB "); debugstring();
		}
		sprintf(string,"%3i, ",Blocks[p].bitmap[0]); debugstring();
	}
	sprintf(string,"\n.BMTab2"); debugstring();
	for (int p=0; p!=BlackCount; ++p)
	{
		if ((p &15) ==0)
		{
			sprintf(string,"\n EQUB "); debugstring();
		}
		sprintf(string,"%3i, ",Blocks[p].bitmap[1]); debugstring();
	}
	sprintf(string,"\n.BMTab3"); debugstring();
	for (int p=0; p!=BlackCount; ++p)
	{
		if ((p &15) ==0)
		{
			sprintf(string,"\n EQUB "); debugstring();
		}
		sprintf(string,"%3i, ",Blocks[p].bitmap[2]); debugstring();
	}
	sprintf(string,"\n.BMTab4"); debugstring();
	for (int p=0; p!=BlackCount; ++p)
	{
		if ((p &15) ==0)
		{
			sprintf(string,"\n EQUB "); debugstring();
		}
		sprintf(string,"%3i, ",Blocks[p].bitmap[3]); debugstring();
	}
	sprintf(string,"\n"); debugstring();

	sprintf(string,"\n.HGTTab1"); debugstring();
	for (int p=0; p!=64; ++p)
	{
		if ((p &15) ==0)
		{
			sprintf(string,"\n EQUB "); debugstring();
		}
		sprintf(string,"%3i, ",heighttab[(p*4)+0]); debugstring();
	}
	sprintf(string,"\n.HGTTab2"); debugstring();
	for (int p=0; p!=64; ++p)
	{
		if ((p &15) ==0)
		{
			sprintf(string,"\n EQUB "); debugstring();
		}
		sprintf(string,"%3i, ",heighttab[(p*4)+1]); debugstring();
	}
	sprintf(string,"\n.HGTTab3"); debugstring();
	for (int p=0; p!=64; ++p)
	{
		if ((p &15) ==0)
		{
			sprintf(string,"\n EQUB "); debugstring();
		}
		sprintf(string,"%3i, ",heighttab[(p*4)+2]); debugstring();
	}
	sprintf(string,"\n.HGTTab4"); debugstring();
	for (int p=0; p!=64; ++p)
	{
		if ((p &15) ==0)
		{
			sprintf(string,"\n EQUB "); debugstring();
		}
		sprintf(string,"%3i, ",heighttab[(p*4)+3]); debugstring();
	}
	sprintf(string,"\n"); debugstring();
}

int main()
{
	file=fopen("log.txt","w");
//  move256intopos();
////	MapSplines32(&map[0]);

	BuildMap();
 Gaps();
// PlotLandscape(&map[0]);

//	return 0;

	BlackCount=0;
	build_blocks_from_map(&MapHeightPos0[0], &MapBlockPos0[0]);
//	sprintf(string,"BlackCount %i\n",BlackCount); debugstring();
	rotate_map();
	build_blocks_from_map(&MapHeightPos1[0], &MapBlockPos1[0]);
//	sprintf(string,"BlackCount %i\n",BlackCount); debugstring();
	rotate_map();
	build_blocks_from_map(&MapHeightPos2[0], &MapBlockPos2[0]);
//	sprintf(string,"BlackCount %i\n",BlackCount); debugstring();
	rotate_map();
	build_blocks_from_map(&MapHeightPos3[0], &MapBlockPos3[0]);
//	sprintf(string,"BlackCount %i\n",BlackCount); debugstring();

//	for (int b=0; b!=BlackCount; ++b)
//	{
//		sprintf(string,"blks, b %3i %3i, %2x, %2x, %2x, %2x\n",BlackCount,b,Blocks[b].bitmap[0], Blocks[b].bitmap[1], Blocks[b].bitmap[2], Blocks[b].bitmap[3]); debugstring();
//	}
	BuildBBCTables();
// todo - build bbc tables
// height0 - 288 bytes
// height1 - 288 bytes
// height2 - 288 bytes
// height3 - 288 bytes

// bitmap00 - 288 bytes
// bitmap01 - 288 bytes
// bitmap02 - 288 bytes
// bitmap03 - 288 bytes

// bitmap10 - 288 bytes
// bitmap11 - 288 bytes
// bitmap12 - 288 bytes
// bitmap13 - 288 bytes

// bitmap20 - 288 bytes
// bitmap21 - 288 bytes
// bitmap22 - 288 bytes
// bitmap23 - 288 bytes

// bitmap30 - 288 bytes
// bitmap31 - 288 bytes
// bitmap32 - 288 bytes
// bitmap33 - 288 bytes


// convert heights 0-63 to zp values 128-254
// write out bitmap tables in bbc format

// read through all off table2 and if its found in table1 ot table3 or table4 then set it to 255

	for (int i=0; i!=256; ++i)
	{
		for (int j=0; j!=256; ++j)
		{
			if (MapHeightPos1[i]==MapHeightPos0[j])
			{
				MapHeightPos1[i]=0;
			}
			if (MapHeightPos1[i]==MapHeightPos2[j])
			{
				MapHeightPos1[i]=0;
			}
			if (MapHeightPos1[i]==MapHeightPos3[j])
			{
				MapHeightPos1[i]=0;
			}
		}
		if (MapHeightPos1[i]!=0)
		{
			sprintf(string,"t2 tst %3i %3i\n",i,MapHeightPos1[i]); debugstring();
			printf("t2 tst %3i %3i\n",i,MapHeightPos1[i]);
		}
	}		
//	{
//		sprintf(string,"blks, b %3i %3i, %2x, %2x, %2x, %2x\n",BlackCount,b,Blocks[b].bitmap[0], Blocks[b].bitmap[1], Blocks[b].bitmap[2], Blocks[b].bitmap[3]); debugstring();
//	}
}
