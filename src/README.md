### 有控制器的分布式

假设

* controller机器IP为192.168.90.1，域名为hatch.auto.local; 
* server1机器IP为192.168.93.53，域名为node22.auto.com; 
* server2机器IP为192.168.93.54, 域名为node23.auto.com
1. 在controller机器上的/etc/hosts中添加server1和server2的DNS
```
192.168.93.53 node22.auto.com
192.168.93.53 node23.auto.com
```
同理在server1和server2上添加controller的DNS
```
192.168.90.1 hatch.auto.local
```

2. 在 controller 机器上,输入以下命令
```
 firewall-cmd --permanent --add-port=4369/tcp
 firewall-cmd --permanent --add-port=4369/udp
 firewall-cmd --permanent --add-port=9000-9012/tcp
 firewall-cmd --permanent --add-port=9000-9012/udp
 firewall-cmd reload
 erl -name controller -setcookie abc -kernel inet_dist_listen_min 9000 inet_dist_listen_max 9012
 # 或者以上步骤改为
 # systemctl stop firewalld
 # erl -name controller -setcookie abc
```
3. 在 server1 机器上，输入命令`erl -name server1 -setcookie abc`,server2机器上输入命令`erl -name server2 -setcookie abc`

4. 测试是否能连接成功，在server1和server2机器的erlang环境中输入`net_adm('controller@hatch.auto.local').`，如果返回`pong`则表示能连接成功。

5. 在controller机器上输入`server:start_controller().`,
在server1机器上输入`server:start_server('controller@hatch.auto.local',2345)`,
在server2机器上输入`server:start_server('controller@hatch.auto.local',2345)`
在client1机器(IP为192.168.93.100,确保能ping通server1和server2)上运行`client:start("192.168.93.53",2345)`则client1连通了server1，并能进行交互。
在client2机器(IP为192.168.93.101,确保能ping通server1和server2)上运行`client:start("192.168.93.54",2345)`则client2连通了server2，并能进行交互。

---

### 没有控制器的分布式

假设

* server1机器IP为192.168.93.53，域名为node22.auto.com; 
* server2机器IP为192.168.93.54, 域名为node23.auto.com;
1. 修改server1中的/etc/hosts文件，添加
```
192.168.93.54 node23 node23.auto.com
```
同理修改server2的/etc/hosts文件，添加
```
192.168.93.53 node22 node22.auto.com
```
2. 修改server1中erlang-chatroom/src/.hosts.erlang为
```
'node23'. 
'node23.auto.com'.
^(new line)
```

同理修改server2中的erlang-chatroom/src/.hosts.erlang为
```
'node22'. 
'node22.auto.com'.
^(new line)
```

3. 启动server1和server2

在server1中输入
```
erl -sname server1 -setcookie abc
parallel_server:start(2345).
```

在server2中输入
```
erl -sname server2 -setcookie abc
parallel_server:start(3456). 
```

4. 启动client
```
erl
client:start("192.168.93.53",2345).
client:start("192.168.93.54",3456).
```
