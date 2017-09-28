#ifndef MSGR_DEFS
#define MSGR_DEFS "Stuff the user needs"

////////////////////////////////////////////////////////////
//
//	Declaration of function types the user will need.
//
////////////////////////////////////////////////////////////
//
typedef void (*MessageFunc)(void *, char *, int);
typedef void (*InputFunc)(void *, char *, int);


////////////////////////////////////////////////////////////
//
//	Declaration of messager functions.
//
////////////////////////////////////////////////////////////
//
int msg_get_socket();
int msg_get_priority_socket();
int msg_readblocking();
int msg_syncblocking();
void msg_readblocking(int);
void msg_syncblocking(int);
int msg_set_groupname(char *);
void *msg_find_resrc(char *, char *);
void *msg_find_resrc(char *);
void *msg_register_resrc(char *, char *, void *);
void *msg_register_resrc(char *, void *);
void *msg_register_resrc_high(char *, char *, void *);
void *msg_register_resrc_high(char *, void *);
int msg_unregister_resrc(char *, char *);
int msg_unregister_resrc(char *);
int msg_set_message(void *, char *, MessageFunc);
int msg_read_message();
int msg_read_message(char *, char *, char *, char **, int *);
int msg_read_message(char *, char *, char **, int *);
int msg_send_message(char *, char *, char *, char *, int);
int msg_send_message(char *, char *, char *, int);

#endif
