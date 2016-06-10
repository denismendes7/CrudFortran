
program main
        type Aluno
            integer matricula
            character nome*20
        end type Aluno

        type Professor
            integer codigo
            character nome*20
        end type Professor

        type Disciplina
            integer codigo
            character nome*20
        end type Disciplina

        type Turma
            integer codigo
            type(Aluno), dimension(5) :: alunos
            type(Professor) :: professor
            type(Disciplina) :: disciplina
            !integer cod_disciplina
        end type Turma

        !Listas
        type(Aluno), dimension(10) :: listaAlunos
        type(Professor), dimension(10) :: listaProfessores
        type(Disciplina), dimension(10) :: listaDisciplinas
        type(Turma), dimension(10) :: listaTurmas

        !Tipos
        type(Aluno) :: a
        type(Professor) :: p
        type(Disciplina) :: d
        type(Turma) :: t

        integer ::prox_aluno = 1, prox_professor = 1, prox_disciplina = 1, prox_turma = 1 ! Porque os arrays em fortran começam em 1
        integer ::totalAlunos = 0, totalProfessores = 0, totalDisciplinas = 0,totalTurma = 0
        integer ::posicaot = 0;
        integer ::num_menu = -1, num_submenu = -1, posicao = -1, num_aluno = 1

        !Programa com parâmetros de entrada e saída que pode ser chamada de dentro do
        !programa principal com o comando CALL
        !call inicializarListas()


        do while(num_menu /= 0)
            write(*,*)""
            write(*,*) "-------Menu--------"
            write(*,*) "Aluno      ->1 "
            write(*,*) "Professor  ->2 "
            write(*,*) "Disciplina ->3 "
            write(*,*) "Turma      ->4 "
            write(*,*) "Sair       ->5 "
            write(*,*) "-------------------"

            write(*,*) "Escolha a opcao desejada"
            read(*,*) num_menu

            call system('CLS')
            write(*,*) "------------------------------"
            write(*,*) "Voltar ao menu principal ->0"
            write(*,*) "Inserir                  ->1"
            write(*,*) "Remover                  ->2"
            write(*,*) "Editar                   ->3"
            write(*,*) "Buscar                   ->4"
            write(*,*) "Listar                   ->5"
            write(*,*) "------------------------------"

            write(*,*) "Escolha a opcao desejada"
            read(*,*) num_submenu

            menu : select case(num_submenu)
                case (1)
                    write(*,*) "Digite o codigo: "
                    if(num_menu .EQ. 1) then
                        read(*,*) a%matricula

                         if(buscarAluno(a) .NE. -1) then
                           call system('CLS')
                           write(*,*) "Erro: Ha um aluno ja cadastrado com o codigo informado!"
                        else
                            write(*,*) "Digite o nome do aluno: "
                            read(*,*) a%nome

                            !Programa com parâmetros de entrada e saída que pode ser chamada de dentro do
                            !programa principal com o comando CALL
                            call system('CLS')
                            call inserirAluno(a)
                        end if
                    elseif(num_menu .EQ. 2) then
                        read(*,*) p%codigo

                        if(buscarProfessor(p) .NE. -1) then
                           call system('CLS')
                           write(*,*) "Erro: Ha um professor ja cadastrado com o codigo informado!"
                        else
                            write(*,*) "Digite o nome do professor"
                            read(*,*) p%nome
                            call system('CLS')
                            call inserirProfessor(p)
                        end if
                    elseif(num_menu .EQ. 3) then
                        read(*,*) d%codigo

                        if(buscarDisciplina(d) .NE. -1) then
                           call system('CLS')
                           write(*,*) "Erro: Ha uma disciplina ja cadastrada com o codigo informado!"
                        else
                            write(*,*) "Digite o nome da disciplina"
                            read(*,*) d%nome
                            call system('CLS')
                            call inserirDisciplina(d)
                        end if

                    elseif(num_menu .EQ. 4) then
                        read(*,*) t%codigo

                        if(buscarTurma(t) .NE. -1) then
                           call system('CLS')
                           write(*,*) "Erro: Ha uma turma ja cadastrada com o codigo informado!"
                        else
                           call system('CLS')
                           call inserirTurma(t)
                        end if
                    end if

                case (2)
                    write(*,*) "Digite o codigo do registro que deseja excluir: "
                    if(num_menu .EQ. 1) then !(.EQ. igual a)
                        read(*,*) a%matricula
                        posicao = buscarAluno(a)

                        if(posicao .EQ. -1) then
                            call system('CLS')
                            write(*,*)"Erro: Aluno nao encontrado!"
                        else
                            call system('CLS')
                            call excluirAluno(posicao)
                        end if
                    elseif(num_menu .EQ. 2) then
                        read(*,*) p%codigo

                        posicao = buscarProfessor(p)

                        if(posicao .EQ. -1) then
                            call system('CLS')
                            write(*,*)"Erro: Professor nao encontrado!"
                        else
                            call system('CLS')
                            call excluirProfessor(posicao)
                        end if
                    elseif(num_menu .EQ. 3) then
                        read(*,*) d%codigo

                        posicao = buscarDisciplina(d)

                        if(posicao .EQ. -1) then
                            call system('CLS')
                            write(*,*)"Erro: Disciplina nao encontrada!"
                        else
                            call system('CLS')
                            call excluirDisciplina(posicao)
                        end if

					elseif(num_menu .EQ. 4) then
                        read(*,*) t%codigo

                        posicao = buscarTurma(t)

                        if(posicao .EQ. -1) then
                            call system('CLS')
                            write(*,*)"Erro: Turma nao encontrada!"
                        else
                            call system('CLS')
                            call excluirTurma(posicao)
                        end if
                    end if
                case (3)
                    write(*,*) "Digite o codigo do registro que deseja editar: "
                    if(num_menu .EQ. 1) then
                        read(*,*) a%matricula
                        posicao = buscarAluno(a)

                        if(posicao .EQ. -1) then
                            call system("CLS")
                            write(*,*)"Erro: Aluno nao encontrado!"
                        else
                            a = listaAlunos(posicao)!passa para a estrutura alunos a posicao da lista de alunos

                            write(*,*)"Aluno Encontrado:"
                            write(*,*)"Codigo: ", a%matricula
                            write(*,*)"Nome: ", a%nome
                            write(*,*)"Digite o novo nome:"
                            read(*,*)a%nome

                            call system("CLS")
                            call editarALuno(a)
                        end if
                    elseif(num_menu .EQ. 2) then
                        read(*,*) p%codigo

                        posicao = buscarProfessor(p)

                        if(posicao .EQ. -1) then
                            call system("CLS")
                            write(*,*)"Erro: Professor nao encontrado!"
                        else
                            p = listaProfessores(posicao)

                            write(*,*)"Professor Encontrado:"
                            write(*,*)"Codigo: ", p%codigo
                            write(*,*)"Nome: ", p%nome
                            write(*,*)"Digite o novo nome:"
                            read(*,*)p%nome

                            call system("CLS")
                            call editarProfessor(p)
                        end if
                    elseif(num_menu .EQ. 3) then
                        read(*,*) d%codigo

                        posicao = buscarDisciplina(d)

                        if(posicao .EQ. -1) then
                            call system("CLS")
                            write(*,*)"Erro: Disciplina nao encontrada!"
                        else
                            d = listaDisciplinas(posicao)

                            write(*,*)"Disciplina encontrada!"
                            write(*,*)"Codigo: ", d%codigo
                            write(*,*)"Nome: ", d%nome
                            write(*,*)"Digite o novo nome:"
                            read(*,*) d%nome

                            call system("CLS")
                            call editarDisciplina(d)
                        end if

					elseif(num_menu .EQ. 4) then
                        read(*,*) t%codigo

                        posicao = buscarTurma(t)

                        if(posicao .EQ. -1) then
                            call system("CLS")
                            write(*,*)"Erro: Turma nao encontrada!"
                        else
                            t = listaTurmas(posicao)

                            write(*,*)"Turma encontrada!"
                            write(*,*)"Codigo: ", t%codigo
                            !write(*,*)"Nome: ", t%nome
                            !write(*,*)"Digite a novo turma:"
                            !read(*,*) t%nome

                            call system("CLS")
                            call editarTurma(t)
                        end if
                    end if
                case (4)
                    write(*,*) "Digite o codigo que deseja buscar: "

                    if(num_menu .EQ. 1) then
                        read(*,*) a%matricula

                        posicao = buscarAluno(a)
                        if (buscarAluno(a) .EQ. -1) then
                            write(*,*) "Aluno nao encontrado!"
                        else
                            write(*,*)"Codigo: ", listaAlunos(posicao)%matricula
                            write(*,*)"Nome: ", listaAlunos(posicao)%nome
                        end if
                    elseif(num_menu .EQ. 2) then
                         read(*,*) p%codigo

                        posicao = buscarProfessor(p)
                        if (buscarProfessor(p) .EQ. -1) then
                            write(*,*) "Professor nao encontrado!"
                        else
                            write(*,*)"Codigo: ", listaProfessores(posicao)%codigo
                            write(*,*)"Nome: ", listaProfessores(posicao)%nome
                        end if
                    elseif(num_menu .EQ. 3) then
                         read(*,*) d%codigo

                        posicao = buscarDisciplina(d)
                        if (buscarDisciplina(d) .EQ. -1) then
                            write(*,*) "Disciplina nao encontrada!"
                        else
                            write(*,*)"Codigo: ", listaDisciplinas(posicao)%codigo
                            write(*,*)"Nome: ", listaDisciplinas(posicao)%nome
                        end if

                    elseif(num_menu .EQ. 4) then
                         read(*,*) t%codigo

                        posicao = buscarTurma(t)
                        if (buscarTurma(t) .EQ. -1) then
                            write(*,*) "Turma nao encontrada!"
                        else
                            write(*,*)"Codigo: ", listaTurmas(posicao)%codigo
                            !write(*,*)"Nome: ", listaTurmas(posicao)%nome
                        end if
                    end if
                case(5)
                    if(num_menu .EQ. 1) then
                        call listarAlunos()
                    elseif(num_menu .EQ. 2) then
                        call listarProfessores()
                    elseif(num_menu .EQ. 3) then
                        call listarDisciplinas()
					elseif(num_menu .EQ. 4) then
                        call listarTurma()
                    end if
                case default

            end select menu

        end do
        contains

!!!!!!!!!!!!!!!!!!!!!!!!!!INSERIR!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine inserirAluno(a)
            implicit none ! Requer que todas as variáveis sejam declaradas
            type(Aluno), intent(inout) :: a
                if(totalAlunos .LT. 10) then ! enquanto total alunos for menor que o tamanho da lista de alunos
                    listaAlunos(prox_aluno) = a !listaAlunos na posicao 1 recebe a estrutura aluno

                    prox_aluno = prox_aluno + 1     !toda vez que essa subrotina é chamada prox_aluno é incrementado
                    totalAlunos = totalAlunos + 1   !toda vez que essa subrotina é chamada total alunos é incrementado

                    write(*,*)"Sucesso: Aluno inserido!"
                    call listarAlunos()
                else
                    write(*,*)"Erro: Nao ha vagas!"
                end if
        end subroutine inserirAluno

        subroutine inserirProfessor(p)
            implicit none
            type(Professor), intent(inout) :: p
                if(totalProfessores .LT. size(listaProfessores)) then
                    listaProfessores(prox_professor) = p

                    prox_professor = prox_professor + 1
                    totalProfessores = totalProfessores + 1

                    write(*,*)"Sucesso: Professor inserido!"

                    call listarProfessores()
                else
                    write(*,*)"Erro: Nao ha vagas!"
                end if
        end subroutine inserirProfessor

        subroutine inserirDisciplina(d)
            implicit none
            type(Disciplina), intent(inout) :: d
                if(totalDisciplinas .LT. size(listaDisciplinas)) then
                    listaDisciplinas(prox_disciplina) = d

                    prox_disciplina = prox_disciplina + 1
                    totalDisciplinas = totalDisciplinas + 1

                    write(*,*)"Sucesso: Disciplina inserida!"

                    call listarDisciplinas()
                else
                    write(*,*)"Erro: Nao foi possivel inserir a disciplina!"
                end if
        end subroutine inserirDisciplina

         !INSERIR TURMA
        subroutine inserirTurma(t)
            implicit none
            type(Turma), intent(inout) :: t
            integer ::contador = 0
            integer al
            integer pr
            integer dis

                if(totalTurma .LT. size(listaTurmas)) then
                    listaTurmas(prox_turma) = t

                   do while(num_aluno /= 0)
                        write(*,*) "insira o numero do aluno"
                        read(*,*) al
                        listaTurmas(totalTurma)%alunos(contador)%nome = listaAlunos(al)%nome
                        contador = contador + 1
                        write(*,*) "deseja inseri mais um aluno sim(1) nao(0)"
                        read(*,*) num_aluno
                   end do

                   num_aluno = 1

                    write(*,*) "insira o numero do professor"
                    read(*,*) pr
                    listaTurmas(totalTurma)%professor%nome = listaProfessores(pr)%nome

                    write(*,*) "insira o numero da disciplina"
                    read(*,*) dis
                    listaTurmas(totalTurma)%disciplina%nome = listaDisciplinas(dis)%nome

                    prox_turma = prox_turma + 1
                    totalTurma = totalTurma + 1


                    write(*,*)"Sucesso: Turma inserida!"

                    call listarTurma()
                else
                    write(*,*)"Erro: Nao foi possivel inserir a disciplina!"
                end if
        end subroutine inserirTurma

!!!!!!!!!!!!!!!!!!!!!!!!!!INSERIR!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!EXCLUIR!!!!!!!!!!!!!!!!!!!!!!!
        !Recebe a posição do aluno a ser excluido
        !Cria um aluno aux e passa para ele a posição do aluno que vai ser excuido mais 1
        !Ou seja o aluno a ser excluido é substituido pelo aluno  que esta a sua frente
        !Depois exclui o item repetido da lista

		subroutine excluirAluno(posicao)
            implicit none
            integer, intent(inout) :: posicao
            integer :: contador
            type(Aluno) :: aluno_aux

            write(*,*) posicao

            do contador = posicao, totalAlunos, +1
                if(contador .EQ. size(listaAlunos)) then
                    aluno_aux%matricula = -1
                    aluno_aux%nome = ''
                else
                    aluno_aux = listaAlunos(contador + 1)
                end if

                listaAlunos(contador) = aluno_aux
            end do

            prox_aluno = prox_aluno - 1
            totalAlunos = totalAlunos - 1

            write(*,*) "Sucesso: Aluno excluido!"

            call listarAlunos()
        end subroutine excluirAluno

        subroutine excluirProfessor(posicao)
            implicit none
            integer, intent(inout) :: posicao
            integer :: contador
            type(Professor) :: professor_aux

            do contador = posicao, totalProfessores, +1
                if(contador .EQ. size(listaProfessores)) then
                    professor_aux%codigo = -1
                    professor_aux%nome = ''
                else
                    professor_aux = listaProfessores(contador + 1)
                end if

                listaProfessores(contador) = professor_aux
            end do
            prox_professor = prox_professor - 1
            totalProfessores = totalProfessores - 1
            write(*,*) "Sucesso: Professor excluido!"
            call listarProfessores()
        end subroutine excluirProfessor

        subroutine excluirDisciplina(posicao)
            implicit none
            integer, intent(inout) :: posicao
            integer :: contador
            type(Disciplina) :: disciplina_aux

            do contador = posicao, totalDisciplinas, +1
                if(contador .EQ. size(listaDisciplinas)) then
                    disciplina_aux%codigo = -1
                    disciplina_aux%nome = ''
                else
                    disciplina_aux = listaDisciplinas(contador + 1)
                end if

                listaDisciplinas(contador) = disciplina_aux
            end do

            prox_disciplina = prox_disciplina - 1
            totalDisciplinas = totalDisciplinas - 1

            write(*,*) "Sucesso: Disciplina excluida!"

            call listarDisciplinas()
        end subroutine excluirDisciplina

        subroutine excluirTurma(posicao)
            implicit none
            integer, intent(inout) :: posicao
            integer :: contador
            type(Turma) :: turma_aux

            do contador = posicao, totalTurma, +1
                if(contador .EQ. size(listaTurmas)) then
                    turma_aux%codigo = -1
!                    turma_aux%nome = ''
                else
                    turma_aux = listaTurmas(contador + 1)
                end if

                listaTurmas(contador) = turma_aux
            end do

            prox_turma = prox_turma - 1
            totalTurma = totalTurma - 1

            write(*,*) "Sucesso: Turma excluida!"

            call listarTurma()
        end subroutine excluirTurma
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!EXCLUIR!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!EDITAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine editarALuno(a)
            implicit none
            type(Aluno), intent(inout) :: a

            listaAlunos(posicao) = a ! lista de aluno recebe o na posicao da busca o novo aluno com a alteracao

            write(*,*)"Sucesso: Aluno editado!"

            call listarAlunos()
        end subroutine editarALuno

        subroutine editarProfessor(p)
            implicit none
            type(Professor), intent(inout) :: p

            listaProfessores(posicao) = p

            write(*,*)"Sucesso: Professors editado!"

            call listarProfessores()
        end subroutine editarProfessor

        subroutine editarDisciplina(d)
            implicit none
            type(Disciplina), intent(inout) :: d

            listaDisciplinas(posicao) = d

            write(*,*)"Sucesso: Disciplina editada!"

            call listarDisciplinas()
        end subroutine editarDisciplina

        subroutine editarTurma(t)
            implicit none
            type(Turma), intent(inout) :: t

            listaTurmas(posicao) = t

            write(*,*)"Sucesso: Turma editada!"

            call listarTurma()
        end subroutine editarTurma

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!EDITAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!BUSCAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        function buscarAluno(a)
            implicit none
            type(Aluno), intent(in) :: a
            integer  buscarAluno
            integer  contador
            buscarAluno = -1

            do contador = 1, totalAlunos, +1
                if(a%matricula == listaAlunos(contador)%matricula) then
                    buscarAluno = contador
                end if
            end do

            return
        end function buscarAluno

         function buscarProfessor(p)
            implicit none
            type(Professor), intent(in) :: p
            integer  buscarProfessor
            integer  contador
            buscarProfessor = -1

            do contador = 1, totalProfessores, +1
                if(p%codigo == listaProfessores(contador)%codigo) then
                    buscarProfessor = contador
                end if
            end do

            return
        end function buscarProfessor

        function buscarDisciplina(d)
            implicit none
            type(Disciplina), intent(in) :: d
            integer  buscarDisciplina
            integer  contador
            buscarDisciplina = -1

            do contador = 1, totalDisciplinas, +1
                if(d%codigo == listaDisciplinas(contador)%codigo) then
                    buscarDisciplina = contador
                end if
            end do

            return
        end function buscarDisciplina


        function buscarTurma(t)
            implicit none
            type(Turma), intent(in) :: t
            integer buscarTurma
            integer contador
            buscarTurma = -1

            do contador = 1, totalTurma, +1
                if(t%codigo == listaTurmas(contador)%codigo) then
                    buscarTurma = contador
                end if
            end do

            return
        end function buscarTurma
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!BUSCAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!LISTAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine listarAlunos()
            implicit none
            integer contador

            write(*,*)""
            write(*,*) "*** ALUNOS *** "

            do contador = 1, totalAlunos, +1
                write(*,*) "No: ", contador
                write(*,*) "Matricula: ", listaAlunos(contador)%matricula
                write(*,*) "Nome: ", listaAlunos(contador)%nome
                write(*,*) "....................................."
            end do

            write(*,*) "TOTAL: ", totalAlunos
        end subroutine listarAlunos

        subroutine listarProfessores()
            implicit none
            integer contador

            write(*,*)""
            write(*,*) "*** PROFESSORES *** "

            do contador = 1, totalProfessores, +1
                write(*,*) "No: ", contador
                write(*,*) "Codigo: ", listaProfessores(contador)%codigo
                write(*,*) "Nome: ", listaProfessores(contador)%nome
                write(*,*) "....................................."
            end do

            write(*,*) "TOTAL: ", totalProfessores
        end subroutine listarProfessores

        subroutine listarDisciplinas()
            implicit none
            integer contador

            write(*,*)""
            write(*,*) "*** DISCIPLINAS *** "

            do contador = 1, totalDisciplinas, +1
                write(*,*) "No: ", contador
                write(*,*) "Codigo: ", listaDisciplinas(contador)%codigo
                write(*,*) "Nome: ", listaDisciplinas(contador)%nome
                write(*,*) "....................................."
            end do

            write(*,*) "TOTAL: ", totalDisciplinas
        end subroutine listarDisciplinas


        subroutine listarTurma()
            implicit none
            integer contador
            integer cont
            integer ::posicao = 0
            write(*,*)""
            write(*,*) "*** TURMA *** "

            do contador = 0, totalTurma-1, +1
                    !write(*,*) "Codigo: ", listaTurmas(contador)%codigo
                    do cont = 0, 2, +1
                            write(*,*) "Nome do aluno: ", listaTurmas(contador)%alunos(cont)%nome
                    end do
                    cont = 0

                    write(*,*) "No: ", contador
                    Write(*,*) "Nome do professor: ", listaTurmas(contador)%professor%nome
                    write(*,*) "Nome da disciplina: ", listaTurmas(contador)%disciplina%nome

                    write(*,*) "....................................."
            end do
            contador = 0

            write(*,*) "TOTAL: ", totalTurma
        end subroutine listarTurma

end program main

