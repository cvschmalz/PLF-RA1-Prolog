# PLF-RA1-Prolog

Instituição: PUCPR

Disciplina: Programação Lógica e Funcional

Professor: Frank Coelho de Alcantara


Alunos: Cecília Lucchesi Mardegan (usuário: ceciLcchM), Christine von Schmalz (usuário: cvschmalz), Erick Maestri de Souza (usuário: ErickMS18)


## Como executar

### No SWISH:

  Copiar e colar o conteúdo de `base_conhecimento.pl` e `main.pl` no editor do SWISH.

- **Modo interativo:**

  Digite o comando: 

  `?- iniciar.`

- **Modo teste:**
  
  O SWISH não permite o uso de `consult/1` para carregar arquivos separados, então fizemos uma versão local.
  
  Para rodar:
  
  `?- teste_local(Id).`
  
  Com `Id` sendo o ID do perfil a ser testado.



### No SWI-Prolog:

  Coloque os arquivos `base_conhecimento.pl`, `main.pl` e todos os `perfil_teste_X` em uma mesma pasta.

  Inicie o Prolog e carregue o arquivo main:

  `?- consult('main.pl').`
  
- **Modo interativo:**
  
  `?- iniciar.`

- **Modo teste:**
  
  `?- teste('perfil_teste_X.pl').`
  
  Com `X` sendo o ID do perfil a ser testado.

