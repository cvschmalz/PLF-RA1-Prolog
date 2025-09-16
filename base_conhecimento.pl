% Entidades

interesse(estatistica).
interesse(inovacao).
interesse(machine_learning).
interesse(design).
interesse(redes).
interesse(ciberseguranca).
interesse(negocios).
interesse(hardware).

pessoa(organizada).
pessoa(analitica).
pessoa(criativa).
pessoa(comunicativa).
pessoa(detalhista).
pessoa(curiosa).
pessoa(questionadora).

aprendizagem(visual).
aprendizagem(pratica).


% Trilhas: NOME, DESCRIÇÂO

trilha(inteligencia_artificial, 'Desenvolvimento de sistemas e modelos que simulam a inteligência humana para aprendizado, raciocínio e tomada de decisão.').
trilha(desenvolvimento_web, 'Criação e construção de sites, aplicações web e serviços, focando na experiência do usuário e funcionalidade.').
trilha(ciberseguranca, 'Proteção de sistemas, redes e dados contra ataques digitais, garantindo a confidencialidade, integridade e disponibilidade da informação.').
trilha(ciencia_de_dados, 'Análise e interpretação de grandes volumes de dados para extrair conhecimento.').
trilha(redes_e_infraestrutura, 'Projeto, implementação e gestão da conectividade e dos sistemas que formam a base de operações de TI.').
trilha(pesquisador_cientifico, 'Investigação e exploração de novos conhecimentos e tecnologias através do método científico para avançar as fronteiras da computação.').

% Características de Perfil: NOME, CARACTERÌSTICA, PESO

perfil(inteligencia_artificial, pessoa(organizada), 1).
perfil(inteligencia_artificial, interesse(estatistica), 2).
perfil(inteligencia_artificial, pessoa(analitica), 3).
perfil(inteligencia_artificial, interesse(inovacao), 4).
perfil(inteligencia_artificial, interesse(machine_learning), 5).

perfil(desenvolvimento_web, aprendizagem(visual), 1).
perfil(desenvolvimento_web, pessoa(criativa), 2).
perfil(desenvolvimento_web, interesse(design), 3).
perfil(desenvolvimento_web, pessoa(comunicativa), 4).
perfil(desenvolvimento_web, pessoa(detalhista), 5).

perfil(ciberseguranca, pessoa(organizada), 1).
perfil(ciberseguranca, interesse(redes), 2).
perfil(ciberseguranca, pessoa(comunicativa), 3).
perfil(ciberseguranca, pessoa(analitica), 4).
perfil(ciberseguranca, interesse(ciberseguranca), 5).

perfil(ciencia_de_dados, interesse(negocios), 1).
perfil(ciencia_de_dados, pessoa(organizada), 2).
perfil(ciencia_de_dados, interesse(estatistica), 3).
perfil(ciencia_de_dados, interesse(machine_learning), 4).
perfil(ciencia_de_dados, pessoa(analitica), 5).

perfil(redes_e_infraestrutura, aprendizagem(pratica), 1).
perfil(redes_e_infraestrutura, pessoa(comunicativa), 2).
perfil(redes_e_infraestrutura, interesse(hardware), 3).
perfil(redes_e_infraestrutura, interesse(ciberseguranca), 4).
perfil(redes_e_infraestrutura, interesse(redes), 5).

perfil(pesquisador_cientifico, aprendizagem(pratica), 1).
perfil(pesquisador_cientifico, pessoa(comunicativa), 2).
perfil(pesquisador_cientifico, pessoa(curiosa), 3).
perfil(pesquisador_cientifico, pessoa(questionadora), 4).
perfil(pesquisador_cientifico, interesse(inovacao), 5).

% Perguntas: ID, PERGUNTA, CARACTERISTICA

pergunta(1, 'Sou metódico e gosto de manter meu ambiente de trabalho, meus arquivos e minhas tarefas organizados e com uma rotina clara.', pessoa(organizada)).
pergunta(2,'Tenho interesse e facilidade em trabalhar com números, análise de dados, estatística e modelos matemáticos.',interesse(estatistica)).
pergunta(3,'Gosto de examinar problemas complexos, quebrá-los em partes menores e entender as relações de causa e efeito antes de tomar uma decisão.',pessoa(analitica)). 
pergunta(4,'Me interesso por tecnologias emergentes, novas ideias revolucionárias e projetos que possam mudar o futuro.',interesse(inovacao)). 
pergunta(5,'Me interesso por sistemas que podem aprender com dados, identificar padrões e fazer previsões ou decisões automatizadas.',interesse(machine_learning)).
pergunta(6,'Prefiro aprender assistindo a vídeos, tutoriais ou vendo exemplos gráficos, em vez de apenas ler instruções escritas.',aprendizagem(visual)).
pergunta(7,'Gosto de pensar "fora da caixa", criar novas soluções, designs ou abordagens originais para os desafios.',pessoa(criativa)).
pergunta(8,'Tenho interesse em criação visual, UX/UI (Experiência do Usuário/Interface do Usuário) e na aparência estética de produtos.',interesse(design)). 
pergunta(9,'Tenho facilidade para me comunicar, explicar ideias complexas de forma clara e trabalhar em equipe.',pessoa(comunicativa)).
pergunta(10,'Presto atenção aos pequenos detalhes e busco a perfeição na execução das tarefas, evitando erros por descuido.', pessoa(detalhista)). 
pergunta(11,'Me interesso por como os dispositivos se conectam e comunicam, protocolos de comunicação e infraestrutura de internet.',interesse(redes)). 
pergunta(12,'Me interesso por proteger sistemas, redes e dados contra ameaças, ataques e vulnerabilidades digitais.',interesse(ciberseguranca)).
pergunta(13,'Me interesso por estratégias de mercado, monetização e como a tecnologia gera valor comercial.',interesse(negocios)). 
pergunta(14,'Aprendo melhor fazendo, experimentando e colocando a mão na massa, em vez de apenas estudar a teoria.',aprendizagem(pratica)). 
pergunta(15,'Tenho curiosidade em entender como os componentes físicos de computadores e equipamentos funcionam e se integram.',interesse(hardware)).
pergunta(16,'Tenho um forte desejo de aprender coisas novas, entender como as coisas funcionam e explorar temas diversos.',pessoa(curiosa)). 
pergunta(17,'Sempre busco entender o "porquê" das coisas, questiono premissas e não aceito respostas superficiais.',pessoa(questionadora)).
   
           