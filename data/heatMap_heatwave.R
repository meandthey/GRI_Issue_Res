library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 지점 이름
stations <- c("수원", "동두천", "양평", "이천", "파주")

# 파일 경로
file_path <- "폭염일수 데이터.xlsx"  # 실제 파일명으로 바꾸세요

# 모든 시트에서 연도와 연합계 추출
data_list <- lapply(stations, function(station) {
  df <- read_excel(file_path, sheet = station)
  df <- df %>%
    select(Year = 1, Total = 14) %>%  # A열 = 연도, N열 = 연합계
    mutate(Station = station)
  return(df)
})

# 데이터 병합
all_data <- bind_rows(data_list)

# 히트맵용 데이터 변환
heatmap_data <- all_data %>%
  mutate(Year = as.factor(Year))  # 연도는 factor로 변환

# 히트맵 그리기
ggplot(heatmap_data, aes(x = Year, y = Station, fill = Total)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Total), size = 17) +  # 숫자 표시
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "지점별 연도별 폭염일수 (연합계 기준)", x = "연도", y = "지점", fill = "폭염일수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 35),   # x축 눈금 글씨 크기
    axis.text.y = element_text(size = 35),                         # y축 눈금 글씨 크기
    axis.title.x = element_text(size = 0),                        # x축 라벨(제목) 글씨 크기
    axis.title.y = element_text(size = 0),                        # y축 라벨(제목) 글씨 크기
    plot.title = element_text(size = 14, face = "bold")            # 그래프 제목
  )
