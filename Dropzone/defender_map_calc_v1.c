
// array [256][4][4] ; 256 across by 4 down by 4 scrolled

// calculate 256x4 random numbers for the 256 across by 4 down


// pixel_array[1024][4][4] ;1024 pixels across (between 0 and 3) by 4 down by 4 scrolled

// byte_array[256][4][4] ;256 bytes across by 4 down by 4 scrolled

// 1 - create 1024 x 4 random numbers (between 0 and 3) for the first 'set' of 4 rows
// 2 - rotate 1024 x 4 random numbers from row 1 into row 2
// 3 - rotate 1024 x 4 random numbers from row 2 into row 3
// 4 - rotate 1024 x 4 random numbers from row 3 into row 4
// 5 - convert 1024 row 1 numbers into 256 bytes of bbc colours
// 6 - convert 1024 row 2 numbers into 256 bytes of bbc colours
// 7 - convert 1024 row 3 numbers into 256 bytes of bbc colours
// 8 - convert 1024 row 4 numbers into 256 bytes of bbc colours

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

const void debugstring() { file=fopen("log.txt","a"); fprintf(file,string); fclose(file); }

const void Randomise(unsigned char *map, const int count)
{
	for (int c=0; c!=count; ++c)
	{
		map[c]=rand() &3;
	}
}

const void Copy(const unsigned char *src, unsigned char *dst, const int count)
{
	for (int s=0; s!=count; ++s)
	{
		const int d=(s-1) &1023;
		dst[d]=src[s];
	}
}

const void Output1024(const unsigned char *map, const int count)
{
	sprintf(string,"\nOutput1024\n"); debugstring();
	for (int c=0; c!=count; ++c)
	{
		sprintf(string," %4i",c); debugstring();
	}
	sprintf(string,"\n"); debugstring();
	for (int c=0; c!=count; ++c)
	{
		sprintf(string," %4i",map[c]); debugstring();
	}
	sprintf(string,"\n"); debugstring();
}

const void binary(const unsigned char in, unsigned char *out)
{
	out[0]=out[1]=out[2]=out[3]=out[4]=out[5]=out[6]=out[7]='0';
	if ((in &1) !=0) { out[7]='1'; }
	if ((in &2) !=0) { out[6]='1'; }
	if ((in &4) !=0) { out[5]='1'; }
	if ((in &8) !=0) { out[4]='1'; }
	if ((in &16) !=0) { out[3]='1'; }
	if ((in &32) !=0) { out[2]='1'; }
	if ((in &64) !=0) { out[1]='1'; }
	if ((in &128) !=0) { out[0]='1'; }
	out[8]='\0';
//	sprintf(string,"binary test in %2i out %s\n", in, out); debugstring();
}

const void Output256(const unsigned char *map, const int count)
{
	unsigned char temp[10];
	sprintf(string,"\nOutput256\n"); debugstring();
	for (int c=0; c!=count; ++c)
	{
		sprintf(string," %3i",c); debugstring();
	}
	sprintf(string,"\n"); debugstring();
	for (int c=0; c!=count; ++c)
	{
		sprintf(string," $%02x,",map[c]); debugstring();
	}
//	sprintf(string,"\n"); debugstring();
//	for (int c=0; c!=count; ++c)
//	{
//		binary(map[c], &temp[0]);
//		sprintf(string," %s,",temp); debugstring();
//	}
//	sprintf(string,"\n"); debugstring();
}

const void OutputBinary256(const unsigned char *map, const int count)
{
	unsigned char temp[10];
	sprintf(string,"\nOutput256\n"); debugstring();
	for (int c=0; c!=count; ++c)
	{
		sprintf(string," %3i",c); debugstring();
	}
	sprintf(string,"\n"); debugstring();
	for (int c=0; c!=count; ++c)
	{
		binary(map[c], &temp[0]);
		sprintf(string," %s,",temp); debugstring();
	}
	sprintf(string,"\n"); debugstring();
}

const unsigned char Colour(const unsigned char colour0, const unsigned char colour1, const unsigned char colour2, const unsigned char colour3)
{
	const unsigned char array00[4]={0x00,0x00,0x00,0x00};
	const unsigned char array01[4]={0x08,0x04,0x02,0x01};
	const unsigned char array10[4]={0x80,0x40,0x20,0x10};
	const unsigned char array11[4]={0x88,0x44,0x22,0x11};
	const unsigned char colour = array00[colour0] + array01[colour1] + array10[colour2] + array11[colour3];
//	sprintf(string,"Colour %1i %1i %1i %1i %2x\n",colour0, colour1,colour2,colour3,colour); debugstring();

	return colour;	
}

const void Colours(const unsigned char *src, unsigned char *dst, const int count)
{
	for (int c=0, m=0; c!=count; ++c, m+=4)
	{
		dst[c]=Colour(src[m+0],src[m+1],src[m+2],src[m+3]);
	}
}

const unsigned char Byte(const unsigned char colour0, const unsigned char colour1, const unsigned char colour2, const unsigned char colour3)
{
	const unsigned char array00[4]={0x00,0x00,0x00,0x00};
	const unsigned char array01[4]={0x40,0x10,0x04,0x01}; //01000000 00010000 00000100 00000001
	const unsigned char array10[4]={0x80,0x20,0x08,0x02};
	const unsigned char array11[4]={0xb0,0x30,0x0b,0x03}; //11000000 00110000 00001100 00000011

	const unsigned char colour = array00[colour0] + array01[colour1] + array10[colour2] + array11[colour3];
//	sprintf(string,"Colour %1i %1i %1i %1i %2x\n",colour0, colour1,colour2,colour3,colour); debugstring();

	return colour;	
}

const void Bytes(const unsigned char *src, unsigned char *dst, const int count)
{
	for (int c=0, m=0; c!=count; ++c, m+=4)
	{
		dst[c]=Byte(src[m+0],src[m+1],src[m+2],src[m+3]);
	}
}

int main()
{
	file=fopen("log.txt","w");
//  move256intopos();
////	MapSplines32(&map[0]);
	unsigned char map0[1024], map1[1024], map2[1024], map3[1024];

	Randomise(&map0[0],1024);
	Copy(&map0[0],&map1[0], 1024);
	Copy(&map1[0],&map2[0], 1024);
	Copy(&map2[0],&map3[0], 1024);

	unsigned char byt0[256], byt1[256], byt2[256], byt3[256];
	Bytes(&map0[0], &byt0[0], 256);
	Bytes(&map1[0], &byt1[0], 256);
	Bytes(&map2[0], &byt2[0], 256);
	Bytes(&map3[0], &byt3[0], 256);
	
	unsigned char col0[256], col1[256], col2[256], col3[256];

	Colours(&map0[0], &col0[0], 256);
	Colours(&map1[0], &col1[0], 256);
	Colours(&map2[0], &col2[0], 256);
	Colours(&map3[0], &col3[0], 256);

//	Output1024(&map0[0],1024);
//	OutputBinary256(&byt0[0],256);
	Output256(&col0[0],256);

//	Output1024(&map1[0],1024);
//	Output256(&byt1[0],256);
	Output256(&col1[0],256);

//	Output1024(&map2[0],1024);
//	Output256(&byt2[0],256);
	Output256(&col2[0],256);

//	Output1024(&map3[0],1024);
//	Output256(&byt3[0],256);
	Output256(&col3[0],256);
	return 0;
}
