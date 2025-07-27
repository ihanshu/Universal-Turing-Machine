# 第一阶段：构建Haskell程序
FROM haskell:9.4 AS builder

WORKDIR /app

# 安装dos2unix工具处理文件格式
RUN apt-get update && apt-get install -y dos2unix

# 复制所有Haskell源文件
COPY *.hs .
COPY *.desc .

# 确保文件使用Unix格式
RUN dos2unix *.hs *.desc

# 安装依赖并编译
RUN cabal update && \
    cabal install --lib containers process && \
    ghc --make Main.hs -package containers -package process -o coursework

# 第二阶段：创建运行时环境
FROM debian:bookworm-slim

# 安装运行时依赖
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libgmp10 \
    ncurses-bin \
    && rm -rf /var/lib/apt/lists/*

# 设置TERM环境变量
ENV TERM=xterm

WORKDIR /app

# 从构建阶段复制编译好的程序和描述文件
COPY --from=builder /app/coursework /app/
COPY --from=builder /app/*.desc /app/

# 设置入口点
ENTRYPOINT ["/app/coursework"]

# 默认命令行参数 (可以在运行容器时覆盖)
CMD ["LRTM.desc", "111101", "M1"] 