#include <stdio.h>
#ifdef __cplusplus
extern"C"
{
#endif
void MPI_BCAST(int *p, int Count, int MPI_TYPE, int Source, int MPI_COMM_WORLD, int ierr )
{
printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void mpi_bcast_(int *p, int Count, int MPI_TYPE, int Source, int MPI_COMM_WORLD, int ierr )
{
printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void MPI_SEND(int *p, int Count, int MPI_TYPE, int dest, int tag, int MPI_COMM_WORLD, int ierr )
{
printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void mpi_send_(int *p, int Count, int MPI_TYPE, int dest, int tag, int MPI_COMM_WORLD, int ierr )
{
printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void MPI_RECV(int *p, int Count, int MPI_TYPE, int source, int tag, int MPI_COMM_WORLD, int stat, int ierr )
{
printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void mpi_recv_(int *p, int Count, int MPI_TYPE, int source, int tag, int MPI_COMM_WORLD, int stat, int ierr )
{
printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void MPI_GET_ADDRESS( int *p, int Address, int ierr )
{
printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void mpi_get_address_( int *p, int Address, int ierr )
{
printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
#ifdef __cplusplus
}
#endif
