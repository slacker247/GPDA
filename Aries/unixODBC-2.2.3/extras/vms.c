#include <stdio.h>
#include <descrip.h>
#include <starlet.h>
#include <ssdef.h>
#include <stsdef.h>
#include <lnmdef.h>
#include <libdef.h>
#include <lib$routines>
#include <rmsdef.h>
#include <fabdef.h>
#include <namdef.h>


static int	error_status = SS$_NORMAL;
static char	error_buffer[256];
static char	getenv_buffer[256];

typedef struct 
{
    struct dsc$descriptor_s	fnmdes;
    struct dsc$descriptor_s	imgdes;
    struct dsc$descriptor_s	symdes;

    char			filename[NAM$C_MAXRSS];
} vms_dl;


int vms_dlsym	    (vms_dl	*, void	**, int);
void * lt_dlsym	    (void *, const char *);

void lt_dlinit (void)
{
}

void * lt_dlopen (const char *filename)
{
    vms_dl	*dh;
    int		status;  
    struct FAB	imgfab;  
    struct NAM  imgnam;
    static char defimg[] = "SYS$SHARE:.EXE";  

    if (filename == NULL) 
    {
	error_status = SS$_UNSUPPORTED;
	return NULL;
    }

    dh = (vms_dl *)malloc (sizeof (vms_dl));  
    if (dh == NULL) 
    {
	error_status = SS$_INSFMEM;
	return NULL;
    }

    imgfab = cc$rms_fab;
    imgfab.fab$l_fna = filename;  
    imgfab.fab$b_fns = strlen (filename);
    imgfab.fab$w_ifi = 0;  
    imgfab.fab$l_dna = defimg;
    imgfab.fab$b_dns = sizeof (defimg);  
    imgfab.fab$l_fop = FAB$M_NAM;
    imgfab.fab$l_nam = &imgnam;  
    imgnam = cc$rms_nam;
    imgnam.nam$l_esa = dh->filename;  
    imgnam.nam$b_ess = NAM$C_MAXRSS;
  
    status = sys$parse (&imgfab);  
    if (!($VMS_STATUS_SUCCESS(status)))
    {
	error_status = status;
	return NULL;
    }

    dh->fnmdes.dsc$b_dtype = DSC$K_DTYPE_T;
    dh->fnmdes.dsc$b_class = DSC$K_CLASS_S;
    dh->fnmdes.dsc$a_pointer = imgnam.nam$l_name;
    dh->fnmdes.dsc$w_length = imgnam.nam$b_name;
    dh->imgdes.dsc$b_dtype = DSC$K_DTYPE_T;
    dh->imgdes.dsc$b_class = DSC$K_CLASS_S;
    dh->imgdes.dsc$a_pointer = dh->filename;
    dh->imgdes.dsc$w_length = imgnam.nam$b_esl;  

    /*
    ** Attempt to load a symbol at this stage to
    ** validate that the shared file can be loaded
    */
    lt_dlsym (dh, "Fake_Symbol_Name");
    if (!((error_status ^ LIB$_KEYNOTFOU) & ~7)) error_status = SS$_NORMAL;

    if (!($VMS_STATUS_SUCCESS(error_status)))
    {
	free (dh);
	return NULL;
    }
 
    return dh;
}

int lt_dlclose (void *handle)
{
    free (handle);
    return 0;
}

void * lt_dlsym (void *handle, const char *name)
{
    vms_dl			*dh;
    void			*ptr;
    int				status, flags;

    dh = (vms_dl *)handle;
    if (!dh) return NULL;

    dh->symdes.dsc$b_dtype = DSC$K_DTYPE_T;  
    dh->symdes.dsc$b_class = DSC$K_CLASS_S;
    dh->symdes.dsc$a_pointer = name;  
    dh->symdes.dsc$w_length = strlen (name);

    /* firstly attempt with flags set to 0 case insensitive */
    flags = 0;
    status = vms_dlsym (dh, &ptr, flags);
    if (!($VMS_STATUS_SUCCESS(status)))
    {
	/*
	** Try again with mixed case flag set 
	*/
        flags = 0x10;

	status = vms_dlsym (dh, &ptr, flags);
	if (!($VMS_STATUS_SUCCESS(status)))
	{
	    error_status = status;
	    return NULL;
	}
    }

    return ptr;
}

int vms_dlsym (
    vms_dl	*dh,
    void	**ptr,
    int		flags)
{
    LIB$ESTABLISH (LIB$SIG_TO_RET);
    return LIB$FIND_IMAGE_SYMBOL (&dh->fnmdes, &dh->symdes, ptr, &dh->imgdes, flags);
}


const char *lt_dlerror (void)
{
    struct dsc$descriptor   desc;
    short		    outlen;
    int			    status;

    if (($VMS_STATUS_SUCCESS(error_status))) return NULL;
    
    desc.dsc$b_dtype = DSC$K_DTYPE_T;
    desc.dsc$b_class = DSC$K_CLASS_S;  
    desc.dsc$a_pointer = error_buffer;
    desc.dsc$w_length = sizeof (error_buffer);

    status = sys$getmsg (error_status, &outlen, &desc, 15, 0);  
    if ($VMS_STATUS_SUCCESS(status)) error_buffer[outlen] = '\0';    
    else sprintf (error_buffer, "OpenVMS exit status %8X", error_status);

    error_status = SS$_NORMAL;  

    return (error_buffer);
}


struct itemlist3 {
    unsigned short	cbbuf;
    unsigned short	item;
    void		*buf;
    unsigned short	*pcbbuf;
};

char * getvmsenv (char *symbol)
{
    int			    status;
    unsigned short	    cbvalue;
    $DESCRIPTOR		    (logicalnametable, "LNM$FILE_DEV");
    struct dsc$descriptor_s logicalname;
    struct itemlist3	    itemlist[2];

    logicalname.dsc$w_length = strlen (symbol);
    logicalname.dsc$b_dtype = DSC$K_DTYPE_T;
    logicalname.dsc$b_class = DSC$K_CLASS_S;
    logicalname.dsc$a_pointer = symbol;

    itemlist[0].cbbuf = sizeof (getenv_buffer) -1;
    itemlist[0].item = LNM$_STRING;
    itemlist[0].buf = getenv_buffer;
    itemlist[0].pcbbuf = &cbvalue;

    itemlist[1].cbbuf = 0;
    itemlist[1].item = 0;
    itemlist[1].buf = 0;
    itemlist[1].pcbbuf = 0;

    status = SYS$TRNLNM (0, &logicalnametable, &logicalname, 0, itemlist);
    if (!($VMS_STATUS_SUCCESS(status))) return NULL;

    if (cbvalue > 0)
    {
	getenv_buffer[cbvalue] = '\0';
	return getenv_buffer;
    }

    return NULL;
}

